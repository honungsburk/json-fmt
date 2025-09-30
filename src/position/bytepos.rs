use std::ops::{Add, AddAssign, Sub, SubAssign};

/// A position in a file.
///
/// # Examples
/// ```
/// use grammarsmith::position::BytePos;
/// let pos = BytePos(0);
/// assert_eq!(pos.shift('a'), BytePos(1));
/// ```
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct BytePos(pub usize);

impl BytePos {
    pub fn shift(self, ch: char) -> Self {
        BytePos(self.0 + ch.len_utf8())
    }
}

impl From<usize> for BytePos {
    fn from(value: usize) -> Self {
        BytePos(value)
    }
}

impl From<BytePos> for usize {
    fn from(value: BytePos) -> Self {
        value.0
    }
}

impl Add<BytePos> for BytePos {
    type Output = Self;

    fn add(self, rhs: BytePos) -> Self::Output {
        BytePos(self.0 + rhs.0)
    }
}

impl Add<usize> for BytePos {
    type Output = Self;

    fn add(self, rhs: usize) -> Self::Output {
        BytePos(self.0 + rhs)
    }
}

impl AddAssign<usize> for BytePos {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs;
    }
}

impl AddAssign<BytePos> for BytePos {
    fn add_assign(&mut self, rhs: BytePos) {
        self.0 += rhs.0;
    }
}

impl Sub<BytePos> for BytePos {
    type Output = Self;

    fn sub(self, rhs: BytePos) -> Self::Output {
        BytePos(self.0 - rhs.0)
    }
}

impl Sub<usize> for BytePos {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        BytePos(self.0 - rhs)
    }
}

impl SubAssign<usize> for BytePos {
    fn sub_assign(&mut self, rhs: usize) {
        self.0 -= rhs;
    }
}

impl SubAssign<BytePos> for BytePos {
    fn sub_assign(&mut self, rhs: BytePos) {
        self.0 -= rhs.0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_addition_operations() {
        let mut pos = BytePos(5);

        // Test Add
        assert_eq!(pos + 3, BytePos(8));

        // Test AddAssign
        pos += 3;
        assert_eq!(pos, BytePos(8));
    }

    #[test]
    fn test_subtraction_operations() {
        let mut pos = BytePos(5);

        // Test Sub
        assert_eq!(pos - 3, BytePos(2));

        // Test SubAssign
        pos -= 3;
        assert_eq!(pos, BytePos(2));
    }

    #[test]
    fn test_shift_with_different_chars() {
        let pos = BytePos(0);

        // ASCII character (1 byte)
        assert_eq!(pos.shift('a'), BytePos(1));

        // 2-byte character
        assert_eq!(pos.shift('é'), BytePos(2));

        // 3-byte character
        assert_eq!(pos.shift('€'), BytePos(3));

        // 4-byte character
        assert_eq!(pos.shift('🦀'), BytePos(4));

        // Test consecutive shifts
        let pos2 = BytePos(0);
        let final_pos = pos2.shift('a').shift('€').shift('🦀');
        assert_eq!(final_pos, BytePos(8)); // 1 + 3 + 4 bytes
    }
}
