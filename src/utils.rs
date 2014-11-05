/// An Iterator over multiple iterators, using each in tern until it runs out of values.
///
/// ```
/// use email::utils::MultiIter;
///
/// let v1 = vec![0u, 1, 2, 3];
/// let v2 = vec![4u, 5, 6, 7];
/// let v3 = vec![8u, 9, 10, 11];
///
/// let mut iter = MultiIter::new(vec![
///     v1.iter(), v2.iter(), v3.iter()
/// ]);
///
/// let values: Vec<uint> = iter.map(|&a| a).collect();
/// assert_eq!(values, vec![0u, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]);
/// ```
pub struct MultiIter<'a, V, T: Iterator<V>> {
    iters: Vec<T>,
    current_iter: uint,
}

impl<'a, V, T: Iterator<V>> MultiIter<'a, V, T> {
    pub fn new(iters: Vec<T>) -> MultiIter<'a, V, T> {
        MultiIter {
            iters: iters,
            current_iter: 0u,
        }
    }
}

impl<'a, V, T: Iterator<V>> Iterator<V> for MultiIter<'a, V, T> {
    fn next(&mut self) -> Option<V> {
        if self.iters.len() == 0 {
            return None;
        }

        loop {
            let iter_len = self.iters.len();

            // Get the next value from the current iterator
            let cur_iter = &mut self.iters[self.current_iter];
            let next = cur_iter.next();

            if next.is_some() {
                // Our current still has a value, so return it
                return next
            } else {
                // Otherwise, go to the next iter and continue
                self.current_iter += 1;
                if self.current_iter >= iter_len {
                    // Make sure that we're not going out of bounds...
                    return None;
                }
            }
        }
    }
}

