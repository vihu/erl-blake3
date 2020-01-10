extern crate blake3;

extern crate rustler;

use rustler::{Encoder, Env, Error, Term};
// use rustler::resource::ResourceArc;
use blake3::Hash;

mod atoms {
    rustler::rustler_atoms! {
        atom ok;
        atom error;
    }
}

rustler::rustler_export_nifs!(
    "erl_blake3",
    [
        ("hash", 1, hash)
    ],
    Some(on_load)
);

struct HashResource {
    pub hash: Hash,
}

fn on_load(env: Env, _info: Term) -> bool {
    rustler::resource_struct_init!(HashResource, env);
    // rustler::resource_struct_init!(IterResource, env);
    // rustler::resource_struct_init!(DecoderResource, env);
    true
}

fn hash<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let data: Vec<u8> = args[0].decode()?;
    let hash = blake3::hash(&data);
    Ok((atoms::ok(), hash).encode(env))
}

impl<'a> Encoder for Hash {
    fn encode<'b>(&self, env: Env<'b>) -> Term<'b> {
    }
}
