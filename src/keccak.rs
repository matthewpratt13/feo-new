use tiny_keccak::{Hasher, Sha3};

#[allow(dead_code)]
fn keccak256(input: &[u8]) -> [u8; 32] {
    let mut hasher = Sha3::v256();
    let mut output = [0u8; 32];

    hasher.update(input);
    hasher.finalize(&mut output);

    output
}
