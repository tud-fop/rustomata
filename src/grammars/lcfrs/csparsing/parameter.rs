pub struct GeneratorParameter<W> {
    pub beamdelta: Option<W>,
    pub beamwidth: Option<usize>,
    pub candidates: Option<usize>
}

impl<W> Default for GeneratorParameter<W> {
    fn default() -> Self {
        GeneratorParameter{
            beamwidth: None,
            beamdelta: None,
            candidates: None
        }
    }
}

impl<W> GeneratorParameter<W> {
    pub fn set_beamwidth(&mut self, bw: usize) {
        self.beamwidth = Some(bw)
    }
    pub fn set_delta(&mut self, delta: W) {
        self.beamdelta = Some(delta)
    }
    pub fn set_candidates(&mut self, c: usize) {
        self.candidates = Some(c)
    }
}