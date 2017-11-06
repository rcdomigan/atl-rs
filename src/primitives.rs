use store::Store;

pub fn sub2(_store: &mut Store, args: &[usize]) -> usize {
    args[0] - args[1]
}

pub fn add2(_store: &mut Store, args: &[usize]) -> usize {
    args[0] + args[1]
}

pub fn equal(_store: &mut Store, args: &[usize]) -> usize  {
    (args[0] == args[1]) as usize
}
