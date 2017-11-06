use store::Store;

pub fn make_tuple(store: &mut Store, args: &[usize]) -> usize {
    store.new_tuple(args)
}
