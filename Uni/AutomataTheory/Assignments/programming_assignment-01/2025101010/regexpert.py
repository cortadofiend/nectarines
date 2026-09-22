from dataclasses import dataclass
from typing import Union, List, Tuple, Set, FrozenSet, Optional, Mapping
from sys import stderr, stdin, argv
from functools import reduce


@dataclass(frozen=True)
class LiteralToken:
    """A literal ASCII character."""
    char: str

@dataclass(frozen=True)
class MetacharToken:
    """One of (, ), |, *, +, ?"""
    kind: str 

@dataclass(frozen=True)
class CharClassToken:
    """Represents a character class or range"""
    chars: Set[str]

Token = Union[LiteralToken, MetacharToken, CharClassToken]

def scan_character_class(pattern: str) -> Tuple[Set[str], str]:
    """Takes a regex pattern starting with a character class/range enclosed by brackets. 
       Parses until the first non-escaped bracket is found.
       Returns a tuple with the set of characters represented by the character class or range and the rest of the pattern as its elements."""
    if len(pattern) < 3:
        raise ValueError("Malformed (possibly empty) character class or range.")
    def scan_elements(pattern: str, accumulator: List[str]) -> Tuple[List[str], str]:
        if not pattern:
            raise ValueError("Character class or range was not closed.")
        match pattern[0]:
            case ']':
                return accumulator, pattern[1:]
            case '\\':
                if len(pattern) < 2:
                    raise ValueError("Dangling backslash \\ at end of character class or range.")
                return scan_elements(pattern[2:], accumulator+[pattern[1]])
            case '-':
                return scan_elements(pattern[1:], accumulator+["RANGE"])
            case char:
                return scan_elements(pattern[1:], accumulator+[char]) 
    def expand_ranges(elements: List[str], accumulator: Set[str]) -> Set[str]:
        if not elements:
            return accumulator
        match elements:
            case [start, "RANGE", end, *tail] if len(start) == 1 and len(end) == 1 and ord(start) <= ord(end):
                range_set = {chr(code) for code in range(ord(start), ord(end) + 1)}
                return expand_ranges(tail, accumulator | range_set)
            case [start, "RANGE", end, *tail] if len(start) == 1 and len(end) == 1 and ord(start) > ord(end):
                raise ValueError(f"Invalid character range [{start!r}-{end!r}] found.")
            case ["RANGE", *tail]:
                return expand_ranges(tail, accumulator | {"-"})
            case [head, *tail]:
                return expand_ranges(tail, accumulator | {head})
            case _:
                raise ValueError("Unknown error while parsing for character classes/ranges.")
    raw_elements, rest = scan_elements(pattern[1:], [])
    resolved_class = expand_ranges(raw_elements, set())
    return resolved_class, rest


def scan(pattern: str, accumulator: List[Token]) -> List[Token]:
    if not pattern:
        return accumulator
    match pattern[0]:
        case '\\':
            if len(pattern) < 2:
                raise ValueError("Dangling backslash \\ at end of pattern.")    
            return scan(pattern[2:], accumulator + [LiteralToken(pattern[1])])
        case '[':
            chars, rest = scan_character_class(pattern)
            return scan(rest, accumulator+[CharClassToken(chars)])
        case op if op in {'|', '*', '?', '+', '(', ')'}:
            return scan(pattern[1:], accumulator + [MetacharToken(op)])
        case char:
            if char.isspace():
                raise ValueError(f"Whitespace {repr(char)} found in regular expression.")
            return scan(pattern[1:], accumulator+[LiteralToken(char)])


def lexer(pattern: str) -> List[Token]:
    try:
        return scan(pattern, [])
    except ValueError as err:
        stderr.write(f"Lexing Error: {err}\n")
        raise

@dataclass(frozen=True)
class SugaredLiteralNode:
    char: str

@dataclass(frozen=True)
class SugaredCharClassNode:
    chars: Set[str]

@dataclass(frozen=True)
class SugaredConcatenationNode:
    left: 'SugaredAST'
    right: 'SugaredAST'

@dataclass(frozen=True)
class SugaredUnionNode:
    left: 'SugaredAST'
    right: 'SugaredAST'

@dataclass(frozen=True)
class SugaredStarNode:
    child: 'SugaredAST'

@dataclass(frozen=True)
class SugaredPlusNode:
    child: 'SugaredAST'

@dataclass(frozen=True)
class SugaredOptionalNode:
    child: 'SugaredAST'

SugaredAST = Union[SugaredLiteralNode, SugaredCharClassNode, SugaredConcatenationNode, SugaredUnionNode, SugaredStarNode, SugaredPlusNode, SugaredOptionalNode]

def parse_atom(tokens: List[Token]) -> Tuple[SugaredAST, List[Token]]:
    if not tokens:
        raise ValueError("Expected end of atom, received unexpected end of pattern.")
    match tokens[0]:
        case LiteralToken(char):
            return SugaredLiteralNode(char), tokens[1:]
        case CharClassToken(chars):
            return SugaredCharClassNode(chars), tokens[1:]
        case MetacharToken('('):
            expression_node, rest = parse_expression(tokens[1:])
            if not rest or rest[0] != MetacharToken(')'):
                raise ValueError("Unmatched open left-parenthesis '(' in pattern.")
            return expression_node, rest[1:]
        case MetacharToken(operator):
            raise ValueError(f"Unexpected operator {operator!r} in pattern.")

def parse_quantified(tokens: List[Token]) -> Tuple[SugaredAST, List[Token]]:
    atom_node, rest = parse_atom(tokens)
    if not rest:
        return atom_node, []
    match rest[0]:
        case MetacharToken('*'):
            return SugaredStarNode(atom_node), rest[1:]
        case MetacharToken('?'):
            return SugaredOptionalNode(atom_node), rest[1:]
        case MetacharToken('+'):
            return SugaredPlusNode(atom_node), rest[1:]
        case _:
            return atom_node, rest

def parse_concatenation(tokens: List[Token]) -> Tuple[SugaredAST, List[Token]]:
    quantified_node, rest = parse_quantified(tokens)
    if not rest:
        return quantified_node, []
    def maximal_concatenation_accumulator(node: SugaredAST, tokens: List[Token]) -> Tuple[SugaredAST, List[Token]]:
        if not tokens:
            return node, []
        match tokens[0]:
            case MetacharToken('|') | MetacharToken(')'):
                return node, tokens
            case _:
                next_node, remaining = parse_quantified(tokens)
                return maximal_concatenation_accumulator(SugaredConcatenationNode(node, next_node), remaining)
    return maximal_concatenation_accumulator(quantified_node, rest)

def parse_expression(tokens: List[Token]) -> Tuple[SugaredAST, List[Token]]:
    concatenation_left, rest = parse_concatenation(tokens)
    def maximal_union_accumulator(node: SugaredAST, tokens: List[Token]) -> Tuple[SugaredAST, List[Token]]:
        if not tokens:
            return node, []
        match tokens[0]:
            case MetacharToken('|'):
                concatenation_right, remaining = parse_concatenation(tokens[1:])
                return maximal_union_accumulator(SugaredUnionNode(node, concatenation_right), remaining)
            case _:
                return node, tokens
    return maximal_union_accumulator(concatenation_left, rest)

def parser(tokens: List[Token]) -> SugaredAST:
    if not tokens:
        raise ValueError("Empty regex pattern.")
    try:
        sugared_ast, remaining = parse_expression(tokens)
        if remaining:
            raise ValueError(f"Unparsed tokens remaining: {remaining}")
        return sugared_ast
    except ValueError as err:
        stderr.write(f"Parsing Error: {err}")
        raise

@dataclass(frozen=True)
class EpsilonNode:
    pass

@dataclass(frozen=True)
class LiteralNode:
    char: str

@dataclass(frozen=True)
class ConcatenationNode:
    left: 'AST'
    right: 'AST'

@dataclass(frozen=True)
class UnionNode:
    left: 'AST'
    right: 'AST'

@dataclass(frozen=True)
class StarNode:
    child: 'AST'

AST = Union[EpsilonNode, LiteralNode, ConcatenationNode, UnionNode, StarNode]

def char_class_to_union_node(chars: List[str]) -> AST:
    if not chars:
        return EpsilonNode()
    sorted_chars = sorted(chars)
    def list_to_node(chars: List[str]) -> AST:
        n_characters = len(chars)
        if n_characters == 1:
            return LiteralNode(chars[0])
        mid = n_characters // 2
        return UnionNode(list_to_node(chars[:mid]), list_to_node(chars[mid:]))
    return list_to_node(sorted_chars)
    

def desugar_ast(node: SugaredAST) -> AST:
    match node:
        case SugaredLiteralNode(char):
            return LiteralNode(char)
        case SugaredCharClassNode(chars):
            return char_class_to_union_node(list(chars))
        case SugaredConcatenationNode(left, right):
            return ConcatenationNode(desugar_ast(left), desugar_ast(right))
        case SugaredUnionNode(left, right):
            return UnionNode(desugar_ast(left), desugar_ast(right))
        case SugaredStarNode(child):
            return StarNode(desugar_ast(child))
        case SugaredOptionalNode(child):
            return UnionNode(EpsilonNode(), desugar_ast(child))
        case SugaredPlusNode(child):
            desugared_child = desugar_ast(child)
            return ConcatenationNode(desugared_child, StarNode(desugared_child))
        
        
@dataclass(frozen=True, order=True)
class StateID:
    state_id: int

@dataclass(frozen=True, order=True)
class Symbol:
    char: Optional[str]
    @property
    def is_epsilon(self) -> bool:
        return self.char is None

@dataclass(frozen=True)
class Transition:
    source: StateID
    symbol: Symbol
    destination: StateID

@dataclass(frozen=True)
class NFA:
    states: FrozenSet[StateID]
    alphabet: FrozenSet[Symbol]
    transitions: FrozenSet[Transition]
    start_state: StateID
    accept_state: StateID

def epsilon_node(next_id: int) -> Tuple[NFA, int]:
    s0, s1 = StateID(next_id), StateID(next_id + 1)
    transitions = Transition(source=s0, symbol=Symbol(None), destination=s1)
    return NFA(states=frozenset({s0, s1}), alphabet=frozenset(), transitions=frozenset({transitions}), start_state=s0, accept_state=s1), next_id + 2

def literal_node(char: str, next_id: int) -> Tuple[NFA, int]:
    s0, s1 = StateID(next_id), StateID(next_id + 1)
    transitions = Transition(source=s0, symbol=Symbol(char), destination=s1)
    return NFA(states=frozenset({s0, s1}), alphabet=frozenset({Symbol(char)}), transitions=frozenset({transitions}), start_state=s0, accept_state=s1), next_id + 2

def concatenation_node(left: NFA, right: NFA) -> NFA:
    return NFA(
        states = left.states | right.states,
        alphabet = left.alphabet | right.alphabet,
        transitions = left.transitions | right.transitions | frozenset({Transition(left.accept_state, Symbol(None), right.start_state)}),
        start_state=left.start_state,
        accept_state=right.accept_state
    )

def union_node(left: NFA, right: NFA, next_id: int) -> Tuple[NFA, int]:
    s_start, s_accept = StateID(next_id), StateID(next_id+1)
    new_transitions = frozenset({
        Transition(source=s_start, symbol=Symbol(None), destination=left.start_state),
        Transition(source=s_start, symbol=Symbol(None), destination=right.start_state),
        Transition(source=left.accept_state, symbol=Symbol(None), destination=s_accept),
        Transition(source=right.accept_state, symbol=Symbol(None), destination=s_accept)
    })
    return NFA(
        states = left.states | right.states | frozenset({s_start, s_accept}),
        alphabet = left.alphabet | right.alphabet,
        transitions = left.transitions | right.transitions | new_transitions,
        start_state = s_start,
        accept_state = s_accept
    ), next_id + 2

def star_node(child: NFA, next_id: int) -> Tuple[NFA,int]:
    s_start, s_accept = StateID(next_id), StateID(next_id+1)
    new_transitions = frozenset({
        Transition(source=s_start, symbol=Symbol(None), destination=child.start_state),
        Transition(source=child.accept_state, symbol=Symbol(None), destination=s_accept),
        Transition(source=child.accept_state, symbol=Symbol(None), destination=child.start_state),
        Transition(source=s_start, symbol=Symbol(None), destination=s_accept)
    })
    return NFA(
        states = child.states | frozenset({s_start, s_accept}),
        alphabet=child.alphabet,
        transitions=child.transitions | new_transitions,
        start_state=s_start,
        accept_state=s_accept
    ), next_id+2

def ast_to_nfa(node: AST, next_id: int) -> Tuple[NFA, int]:
    match node:
        case EpsilonNode():
            return epsilon_node(next_id)
        case LiteralNode(char):
            return literal_node(char, next_id)
        case ConcatenationNode(left, right):
            left_node, next_id_1 = ast_to_nfa(left, next_id)
            right_node, next_id_2 = ast_to_nfa(right, next_id_1)
            return concatenation_node(left_node, right_node), next_id_2
        case UnionNode(left, right):
            left_node, next_id_1 = ast_to_nfa(left, next_id + 2)
            right_node, next_id_2 = ast_to_nfa(right, next_id_1)
            union_nfa, _ = union_node(left_node, right_node, next_id)
            return union_nfa, next_id_2
        case StarNode(child):
            child_node, next_id_1 = ast_to_nfa(child, next_id + 2)
            star_nfa, _ = star_node(child_node, next_id)
            return star_nfa, next_id_1

def build_nfa(node: AST) -> NFA:
    nfa, _ = ast_to_nfa(node, 0)
    return nfa

@dataclass(frozen=True, order=True)
class DFAStateID:
    value: int

@dataclass(frozen=True)
class NFASubset:
    states: FrozenSet[StateID]
    @property
    def is_empty(self) -> bool:
        return not bool(self.states)

@dataclass(frozen=True)
class DFATransition:
    source: DFAStateID
    symbol: str
    destination: Optional[DFAStateID]  # None indicates transition to dead state

@dataclass(frozen=True)
class DFA:
    states: FrozenSet[DFAStateID]
    alphabet: FrozenSet[str]
    transitions: FrozenSet[DFATransition]
    start_state: DFAStateID
    accepting_states: FrozenSet[DFAStateID]
    subsets: Mapping[DFAStateID, NFASubset]


def epsilon_step(nfa: NFA, subset: NFASubset) -> NFASubset:
    """Retrieves all NFA states reachable directly via a single epsilon-transition."""
    return NFASubset(frozenset({transition.destination for transition in nfa.transitions if transition.source in subset.states and transition.symbol.is_epsilon}))

def epsilon_closure(nfa: NFA, subset: NFASubset) -> NFASubset:
    """Calculates the full epsilon-closure of a set of NFA states."""
    next_states = subset.states | epsilon_step(nfa, NFASubset(subset.states)).states
    if next_states == subset.states:
        return NFASubset(next_states)
    return epsilon_closure(nfa, NFASubset(next_states))

def char_closure(nfa: NFA, subset: NFASubset, char: str) -> NFASubset:
    """Calculates delta(S, char) for a set of NFA states across a single character."""
    target_symbol = Symbol(char)
    return NFASubset(frozenset({transition.destination for transition in nfa.transitions if transition.source in subset.states and transition.symbol == target_symbol}))

def nfa_to_dfa(nfa: NFA) -> DFA:
    """
    Converts an NFA to a deterministic DFA via subset construction.
    """
    #print(nfa)
    sorted_alphabet = tuple(sorted([symbol.char for symbol in nfa.alphabet if symbol.char is not None]))
    
    start_subset = epsilon_closure(nfa, NFASubset(frozenset({nfa.start_state})))
    start_dfa_id = DFAStateID(0)
    
    subset_to_id : dict[NFASubset, DFAStateID] = {start_subset: start_dfa_id}
    processing_list: List[Tuple[(NFASubset, DFAStateID)]] = [(start_subset, start_dfa_id)]
    
    def subset_eval_over_char(current_subset: NFASubset, current_dfa_subset_id: DFAStateID, char: str, state: Tuple[List[Tuple[NFASubset, DFAStateID]], dict[NFASubset, DFAStateID], FrozenSet[DFATransition], int]) -> Tuple[List[Tuple[NFASubset, DFAStateID]], dict[NFASubset, DFAStateID], FrozenSet[DFATransition], int]:
        processing_list, subset_to_id, transitions, next_id_val = state
        target_nfa_set = epsilon_closure(nfa, char_closure(nfa, current_subset, char))
        if target_nfa_set.is_empty:
            return processing_list, subset_to_id, transitions, next_id_val
        if target_nfa_set in subset_to_id:
            target_dfa_id = subset_to_id[target_nfa_set]
            new_transition = transitions | frozenset({DFATransition(current_dfa_subset_id, char, target_dfa_id)})
            return processing_list, subset_to_id, new_transition, next_id_val
        target_dfa_id = DFAStateID(next_id_val)
        new_subset_to_id = {**subset_to_id, target_nfa_set: target_dfa_id}
        new_processing_list = processing_list + [(target_nfa_set, target_dfa_id)]
        new_transition = transitions | frozenset({DFATransition(current_dfa_subset_id, char, target_dfa_id)})
        return new_processing_list, new_subset_to_id, new_transition, next_id_val + 1
        
    def subset_eval_over_alphabet(processing_list: List[Tuple[NFASubset, DFAStateID]], subset_to_id: dict[NFASubset,DFAStateID], transitions: FrozenSet[DFATransition], next_id_val: int) -> Tuple[dict[NFASubset,DFAStateID], FrozenSet[DFATransition]]:
        if not processing_list:
            return subset_to_id, transitions
        current_subset, current_dfa_subset_id = processing_list[0]
        remaining_processing_list = processing_list[1:]
        new_processing_list, new_subset_to_id, new_transitions, new_id_val = reduce(
            lambda state, char: subset_eval_over_char(current_subset, current_dfa_subset_id, char, state), sorted_alphabet, (remaining_processing_list, subset_to_id, transitions, next_id_val) 
        )
        return subset_eval_over_alphabet(new_processing_list, new_subset_to_id, new_transitions, new_id_val)

    subset_to_id, transitions = subset_eval_over_alphabet(processing_list, subset_to_id, frozenset(), 1)
    id_to_subset = {dfa_id:nfa_subset for nfa_subset,dfa_id in subset_to_id.items()}
    accepting_dfa_states = frozenset({dfa_id for dfa_id, subset in id_to_subset.items() if nfa.accept_state in subset.states})
    return DFA(frozenset(subset_to_id.values()), frozenset(sorted_alphabet), transitions, start_dfa_id, accepting_dfa_states, id_to_subset)

def format_dfa_to_csv(dfa: DFA) -> str:
    sorted_alphabet = sorted(dfa.alphabet)
    header = ["State"] + sorted_alphabet
    transition_lookup: dict[tuple[DFAStateID, str], Optional[DFAStateID]] = {(transition.source, transition.symbol): transition.destination for transition in dfa.transitions}

    def format_row_label(state_id: DFAStateID) -> str:
        is_start = (state_id == dfa.start_state)
        is_accepting = (state_id in dfa.accepting_states)
        match (is_start, is_accepting):
            case (True, True):
                prefix = "->*"
            case (True, False):
                prefix = "->"
            case (False, True):
                prefix = "*"
            case (False, False):
                prefix = ""
        return f"{prefix}{state_id.value}"

    def build_row(state_id: DFAStateID) -> str:
        label = format_row_label(state_id)
        def resolve_char(char: str) -> str:
            match transition_lookup.get((state_id, char)):
                case None:
                    return "-"
                case target_id:
                    return f"{target_id.value}"
        return ", ".join([label]+[resolve_char(char) for char in sorted_alphabet])

    sorted_states = sorted(dfa.states, key = lambda s: s.value)
    rows = [", ".join(header)] + [build_row(state_id) for state_id in sorted_states]
    return "\n".join(rows)

def print_debug_dfa(dfa: DFA) -> None:
    stderr.write(format_dfa_to_csv(dfa)+'\n')



def accepts_word(dfa:DFA, word: str) -> bool:
    transition_lookup: dict[tuple[DFAStateID, str], Optional[DFAStateID]] = {(transition.source, transition.symbol): transition.destination for transition in dfa.transitions}
    def transition_step(dfa: DFA, current_state: Optional[DFAStateID], char: str) -> Optional[DFAStateID]:
        if current_state is None:
            return None
        return transition_lookup.get((current_state, char))
    final_state = reduce(lambda state, char: transition_step(dfa, state, char), word, dfa.start_state)
    return final_state is not None and final_state in dfa.accepting_states
    

def run() -> None:
    pattern = input()
    text = stdin.read().split()
    dfa = nfa_to_dfa(build_nfa(desugar_ast(parser(lexer(pattern)))))
    debug = False
    if "--debug" in argv[1:]:
        debug = True
    if debug:
        print_debug_dfa(dfa)
    matching_words = [word for word in text if accepts_word(dfa, word)]
    print("\n".join(matching_words))


if __name__ == "__main__":
    run()