// relabel function for configurations and states
pub trait Relabel<N1, N2, O>{
    fn relabel(&self, fn(N1)-> N2) -> O;
}
