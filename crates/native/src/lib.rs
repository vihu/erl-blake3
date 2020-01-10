extern crate blake3;

extern crate rustler;

use rustler::{Encoder, Env, Error, Term};
use rustler::resource::ResourceArc;
use blake3::{Hash, Hasher};

mod atoms {
    rustler::rustler_atoms! {
        atom ok;
        atom error;
    }
}

rustler::rustler_export_nifs!(
    "erl_blake3",
    [
        ("hash", 1, hash),
        ("as_bytes", 1, as_bytes),
        ("to_hex", 1, to_hex),
        ("new", 0, new),
        ("update", 2, update),
        ("finalize", 1, finalize)
    ],
    Some(on_load)
);

struct HashResource {
    pub hash: Hash,
}

struct HasherResource {
    pub hasher: Hasher,
}

fn on_load(env: Env, _info: Term) -> bool {
    rustler::resource_struct_init!(HashResource, env);
    rustler::resource_struct_init!(HasherResource, env);
    // rustler::resource_struct_init!(DecoderResource, env);
    true
}

fn hash<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let data: Vec<u8> = args[0].decode()?;
    let hash = blake3::hash(&data);
    let res = ResourceArc::new(HashResource {
        hash: hash
    });

    Ok((atoms::ok(), res).encode(env))
}

fn as_bytes<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let hash_resource: ResourceArc<HashResource> = args[0].decode()?;
    let hash = hash_resource.hash;
    let bytes = Hash::as_bytes(&hash);
    let bytes_vec = bytes.to_vec();
    Ok((atoms::ok(), bytes_vec).encode(env))
}

fn to_hex<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let hash_resource: ResourceArc<HashResource> = args[0].decode()?;
    let hash = hash_resource.hash;
    let arr_str = Hash::to_hex(&hash);
    let arr_vec = arr_str.as_str();
    Ok((atoms::ok(), arr_vec).encode(env))
}

fn new<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let hasher = blake3::Hasher::new();
    let res = ResourceArc::new(HasherResource {
        hasher: hasher
    });
    Ok((atoms::ok(), res).encode(env))
}

fn update<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let hasher_resource: ResourceArc<HasherResource> = args[0].decode()?;
    let data: Vec<u8> = args[1].decode()?;
    let mut hasher = hasher_resource.hasher.clone();
    hasher.update(&data);
    let res = ResourceArc::new(HasherResource {
        hasher: hasher
    });
    Ok((atoms::ok(), res).encode(env))
}

fn finalize<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let hasher_resource: ResourceArc<HasherResource> = args[0].decode()?;
    let hasher = hasher_resource.hasher.clone();
    let hash = hasher.finalize();
    let res = ResourceArc::new(HashResource {
        hash: hash
    });
    Ok((atoms::ok(), res).encode(env))
}
