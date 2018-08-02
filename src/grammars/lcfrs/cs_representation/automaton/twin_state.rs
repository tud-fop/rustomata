/// This module contains structures to handle state and index ranges.
/// It is used for the extraction of Dyck words from finite state automata.

use num_traits::One;

/// A range of integer states.
/// This structure is primarily used for two purposes:
/// * to represent a range of states of a finite state automaton (similar to
///   Bar-Hillel's, Perles' and Shamir's construction), and
/// * to represent a range of indices in a word.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct TwinState {
    pub left: usize,
    pub right: usize
}

/// Contains
/// * a range of states in a finite state automaton, and
/// * a range of indices (range).
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct TwinRange {
    pub range: TwinState,
    pub state: TwinState
}


impl TwinRange {
    /// Inserts the target of a `TwinArc` into the state part of a `TwinRange`.
    pub fn apply_state_arc<W>(&self, arc: &TwinArc<W>) -> Self {
        TwinRange{
            range: self.range,
            state: TwinState{ left: arc.left, right: arc.right }
        }
    }

    /// Inserts a `TwinState` into the state part of a `TwinRange`.
    pub fn apply_state(&self, state: TwinState) -> Self {
        TwinRange{ range: self.range, state }
    }

    /// Splits a both components of a `TwinRange` according to a given state
    /// and index. So, it returns two parts, a `TwinRange` to the left of the
    /// given state and index, and one to the right.
    pub fn split(&self, state: usize, range: usize) -> (Self, Self) {
        ( TwinRange{ state: TwinState{ left: self.state.left, right: state }
                   , range: TwinState{ left: self.range.left, right: range }
                   }, 
          TwinRange{ state: TwinState{ left: state, right: self.state.right }
                   , range: TwinState{ left: range, right: self.range.right }
                   }
        )
    }
}

/// Represents two transitions in a finite state automaton over a Dyck alphabet
/// with opposite parentheses (i.e. some transition p₁ → ⟨ q₁ and p₂ → ⟩ q₂).
/// `left` is the origin state of the transition with the opening parenthesis
/// (p₁ in the previous example), right is the target state of the arc with the
/// closing parenthesis (q₂ in the previous example).
/// `label` is an identifier for the concrete parenthesis and the weight is
/// the product of both transisions' weights.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct TwinArc<W> {
    pub left: usize,
    pub label: usize,
    pub weight: W,
    pub right: usize
}

/// A transition of a finite state automaton (without symbol).
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct StateArc<W> {
    pub from: usize, 
    pub weight: W,
    pub to: usize
}

impl<W> TwinArc<W> {
    /// Reconstructs the transitions of the finite state automaton that were
    /// used to construct the `TwinArc`
    pub fn split(&self, from: &TwinState) -> (StateArc<W>, StateArc<W>)
    where
        W: One + Copy
    {
        ( StateArc{ from: self.left, weight: self.weight, to: from.left }
        , StateArc{ from: from.right, weight: W::one(), to: self.right }
        )
    }
}