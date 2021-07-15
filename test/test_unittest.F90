program test_unittest
  use unittest, only : unittest_t
  implicit none

  type(unittest_t) :: test

  call test%init()
  call test%start_case()
  call test%assert_true(2>1, __FILE__, __LINE__)
  call test%assert_false(1<2, __FILE__, __LINE__)
  call test%assert_equal(1, 2, __FILE__, __LINE__)
  call test%assert_equal(1.1, 1.3, __FILE__, __LINE__)
  call test%assert_equal('abc', 'abcd', __FILE__, __LINE__)
  call test%assert_equal(.true., .false., __FILE__, __LINE__)
  call test%end_case()
  call test%start_case()
  call test%assert_false(.true., __FILE__, __LINE__)
  call test%assert_true(.false., __FILE__, __LINE__)
  call test%end_case()
  call test%summary()

end program test_unittest