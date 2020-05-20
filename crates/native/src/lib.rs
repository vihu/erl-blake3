extern crate blake3;

extern crate rustler;

use rustler::{Env, Term};
use rustler::resource::ResourceArc;
use rustler::types::Atom;
use blake3::{Hash, Hasher};

mod atoms;

rustler::init!(
    "erl_blake3",
    [
        hash,
        as_bytes,
        to_hex,
        new,
        update,
        finalize
    ],
    load=on_load
);

struct HashResource {
    pub hash: Hash,
}

struct HasherResource {
    pub hasher: Hasher,
}


fn on_load(env: Env, _info: Term) -> bool {
    rustler::resource!(HashResource, env);
    rustler::resource!(HasherResource, env);
    // rustler::resource_struct_init!(DecoderResource, env);
    true
}

#[rustler::nif]
fn hash(data: Vec<u8>) -> (Atom, ResourceArc<HashResource>) {
    let hash = blake3::hash(&data);
    let res = ResourceArc::new(HashResource {
        hash
    });

    (atoms::ok(), res)
}

#[rustler::nif]
fn as_bytes(hash_resource: ResourceArc<HashResource>) -> (Atom, Vec<u8>) {
    let h = hash_resource.hash;
    let bytes = Hash::as_bytes(&h);
    let bytes_vec = bytes.to_vec();
    (atoms::ok(), bytes_vec)
}

#[rustler::nif]
fn to_hex(hash_resource: ResourceArc<HashResource>) -> (Atom, String) {
    let h = hash_resource.hash;
    let arr_str = Hash::to_hex(&h);
    let arr_vec = arr_str.as_str();
    (atoms::ok(), String::from(arr_vec))
}

#[rustler::nif]
fn new() -> (Atom, ResourceArc<HasherResource>) {
    let hasher = blake3::Hasher::new();
    let res = ResourceArc::new(HasherResource {
        hasher
    });
    (atoms::ok(), res)
}

#[rustler::nif]
fn update(hasher_resource: ResourceArc<HasherResource>, data: Vec<u8>) -> (Atom, ResourceArc<HasherResource>) {
    let mut hasher = hasher_resource.hasher.clone();
    hasher.update(&data);
    let res = ResourceArc::new(HasherResource {
        hasher
    });
    (atoms::ok(), res)
}

#[rustler::nif]
fn finalize(hasher_resource: ResourceArc<HasherResource>) -> (Atom, ResourceArc<HashResource>) {
    let hasher = hasher_resource.hasher.clone();
    let h = hasher.finalize();
    let res = ResourceArc::new(HashResource {
        hash: h
    });
    (atoms::ok(), res)
}
