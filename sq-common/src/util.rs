/// Copy foreign C str to owned String
///
/// # Safety
/// Safe as long as ptr is to normal null-terminated string
pub unsafe fn cstr_to_string(ptr: *const i8) -> String {
    let len = libc::strlen(ptr as _);
    let mut v = Vec::with_capacity(len);
    std::ptr::copy(ptr, v.as_mut_ptr() as _, len);
    v.set_len(len);
    String::from_utf8_unchecked(v)
}
