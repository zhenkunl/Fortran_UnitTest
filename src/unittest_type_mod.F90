module unittest_type
  use string, only : string_t
  use assert_result, only : assert_result_t
  use assert_counter, only : assert_counter_t
  use case_counter, only : case_counter_t
  use case_assert_counter, only : case_assert_counter_t

  implicit none
  private
  public :: unittest_t

  type :: unittest_t
    private
    type(assert_counter_t)      :: assertions
    type(case_counter_t)        :: cases
    type(case_assert_counter_t) :: case_assertions
    type(string_t)              :: name
    integer                     :: number = 0
  contains
    private
    procedure, public, pass(self)  :: init
    procedure, public, pass(self)  :: start_case
    procedure, public, pass(self)  :: end_case
    procedure, public, pass(self)  :: summary

    procedure, public, pass(self)  :: assert_true
    procedure, public, pass(self)  :: assert_false

    generic, public                :: assert_equal => assert_equal_integer, assert_equal_real, assert_equal_logical, &
                                      assert_equal_string, assert_equal_character
    procedure, private, pass(self) :: assert_equal_integer
    procedure, private, pass(self) :: assert_equal_real
    procedure, private, pass(self) :: assert_equal_logical
    procedure, private, pass(self) :: assert_equal_string
    procedure, private, pass(self) :: assert_equal_character

    procedure, private, pass(self) :: header
    procedure, private, pass(self) :: footer
    procedure, private, pass(self) :: assert
  end type unittest_t

contains

  subroutine init(self, name)

    implicit none
    class(unittest_t), intent(inout)       :: self
    character(len=*), intent(in), optional :: name
    type(string_t)                         :: name_

    self%number = self%number + 1
    if (present(name)) then
      self%name = name
    else
      self%name = 'suite' // string_t(self%number)
    end if
    call self%assertions%init()
    call self%cases%init()
    call self%header()

  end subroutine init

  subroutine start_case(self, case_name)

    implicit none
    class(unittest_t), intent(inout)       :: self
    character(len=*), intent(in), optional :: case_name
    type(string_t)                         :: case_name_

    call self%cases%add_total()
    if (present(case_name)) then
      case_name_ = case_name
    else
      case_name_ = 'case' // string_t(self%cases%total_num())
    end if
    call self%case_assertions%init()
    call self%case_assertions%init_name(case_name_)
    call self%case_assertions%header()

  end subroutine start_case

  subroutine end_case(self)

    implicit none
    class(unittest_t), intent(inout) :: self

    if (self%case_assertions%failed_num() > 0) then
      call self%cases%add_failed()
    else
      call self%cases%add_passed()
    end if
    write(*, *) self%case_assertions
    call self%case_assertions%footer()

  end subroutine end_case

  subroutine summary(self)

    implicit none
    class(unittest_t), intent(in) :: self
    character(len=:), allocatable :: name

    name = self%name
    write(*, *) '|'
    write(*, *) '+-> Summary:'
    write(*, '(a)', advance='no') ' |   +-> ' // name // ': '
    write(*, *) self%assertions
    write(*, '(a)', advance='no') ' |   +-> ' // name // ': '
    write(*, *) self%cases
    call self%footer()

  end subroutine summary

  subroutine assert_true(self, condition, file_name, line_number)

    implicit none
    class(unittest_t), intent(inout)       :: self
    logical, intent(in)                    :: condition
    character(len=*), intent(in), optional :: file_name
    integer, intent(in), optional          :: line_number
    type(assert_result_t)                  :: assert_result_
    integer                                :: id

    call self%assert(condition)
    id = self%case_assertions%total_num()
    assert_result_ = assert_result_t(id, string_t(condition), string_t('true'), string_t('N/A'), condition, file_name, line_number)
    write(*, *) assert_result_

  end subroutine assert_true

  subroutine assert_false(self, condition, file_name, line_number)

    implicit none
    class(unittest_t), intent(inout)       :: self
    logical, intent(in)                    :: condition
    character(len=*), intent(in), optional :: file_name
    integer, intent(in), optional          :: line_number
    type(assert_result_t)                  :: assert_result_
    integer                                :: id

    call self%assert(.not. condition)
    id = self%case_assertions%total_num()
    assert_result_ = assert_result_t(id, string_t(condition), string_t('false'), string_t('N/A'), .not. condition, file_name, line_number)
    write(*, *) assert_result_

  end subroutine assert_false

  subroutine assert_equal_integer(self, expected, actual, file_name, line_number)

    implicit none
    class(unittest_t), intent(inout)       :: self
    integer, intent(in)                    :: expected
    integer, intent(in)                    :: actual
    character(len=*), intent(in), optional :: file_name
    integer, intent(in), optional          :: line_number
    type(assert_result_t)                  :: assert_result_
    logical                                :: condition
    integer                                :: id

    condition = expected == actual
    call self%assert(condition)
    id = self%case_assertions%total_num()
    assert_result_ = assert_result_t(id, string_t(expected), string_t('='), string_t(actual), condition, file_name, line_number)
    write(*, *) assert_result_

  end subroutine assert_equal_integer

  subroutine assert_equal_real(self, expected, actual, file_name, line_number)

    implicit none
    class(unittest_t), intent(inout)       :: self
    real, intent(in)                       :: expected
    real, intent(in)                       :: actual
    character(len=*), intent(in), optional :: file_name
    integer, intent(in), optional          :: line_number
    type(assert_result_t)                  :: assert_result_
    logical                                :: condition
    integer                                :: id

    condition = expected == actual
    call self%assert(condition)
    id = self%case_assertions%total_num()
    assert_result_ = assert_result_t(id, string_t(expected), string_t('='), string_t(actual), condition, file_name, line_number)
    write(*, *) assert_result_

  end subroutine assert_equal_real

  subroutine assert_equal_logical(self, expected, actual, file_name, line_number)

    implicit none
    class(unittest_t), intent(inout)       :: self
    logical, intent(in)                    :: expected
    logical, intent(in)                    :: actual
    character(len=*), intent(in), optional :: file_name
    integer, intent(in), optional          :: line_number
    type(assert_result_t)                  :: assert_result_
    logical                                :: condition
    integer                                :: id

    condition = expected .eqv. actual
    call self%assert(condition)
    id = self%case_assertions%total_num()
    assert_result_ = assert_result_t(id, string_t(expected), string_t('='), string_t(actual), condition, file_name, line_number)
    write(*, *) assert_result_

  end subroutine assert_equal_logical

  subroutine assert_equal_string(self, expected, actual, file_name, line_number)

    implicit none
    class(unittest_t), intent(inout)       :: self
    type(string_t), intent(in)             :: expected
    type(string_t), intent(in)             :: actual
    character(len=*), intent(in), optional :: file_name
    integer, intent(in), optional          :: line_number
    type(assert_result_t)                  :: assert_result_
    logical                                :: condition
    integer                                :: id

    condition = expected == actual
    call self%assert(condition)
    id = self%case_assertions%total_num()
    assert_result_ = assert_result_t(id, expected, string_t('='), actual, condition, file_name, line_number)
    write(*, *) assert_result_

  end subroutine assert_equal_string

  subroutine assert_equal_character(self, expected, actual, file_name, line_number)

    implicit none
    class(unittest_t), intent(inout)       :: self
    character(len=*), intent(in)           :: expected
    character(len=*), intent(in)           :: actual
    character(len=*), intent(in), optional :: file_name
    integer, intent(in), optional          :: line_number
    type(assert_result_t)                  :: assert_result_
    logical                                :: condition
    integer                                :: id

    condition = expected == actual
    call self%assert(condition)
    id = self%case_assertions%total_num()
    assert_result_ = assert_result_t(id, string_t(expected), string_t('='), string_t(actual), condition, file_name, line_number)
    write(*, *) assert_result_

  end subroutine assert_equal_character

  subroutine header(self)

    implicit none
    class(unittest_t), intent(in) :: self
    integer, parameter            :: columns = 80
    type(string_t)                :: title
    integer                       :: i, pos1, pos2

    title = 'Report of Suite: ' // self%name

    write(*, *)
    pos1 = (columns - title%len() - 2) * 0.5
    do i = 1, pos1
      write(*, '(a)', advance='no') '/'
    end do
    write(*, "(' ', a, ' ')", advance='no') title%value()
    pos2 = pos1 + title%len() + 2
    do i = pos2, columns
      write(*, '(a)', advance='no') '/'
    end do
    write(*, *)
    write(*, *)
    write(*, *) '+-> Details:'
    write(*, *) '|   |'

  end subroutine header

  subroutine footer(self)

    implicit none
    class(unittest_t), intent(in) :: self
    integer, parameter            :: columns = 80
    integer                       :: i

    associate(unused => self)
    end associate

    write(*, *)
    do i = 1, columns
      write(*, '(a)', advance='no') '/'
    end do
    write(*, *)
    write(*, *)

  end subroutine footer

  subroutine assert(self, condition)

    implicit none
    class(unittest_t), intent(inout) :: self
    logical, intent(in)              :: condition

    if (condition) then
      call self%case_assertions%add_passed()
      call self%assertions%add_passed()
    else
      call self%case_assertions%add_failed()
      call self%assertions%add_failed()
    end if
    call self%case_assertions%add_total()
    call self%assertions%add_total()

  end subroutine assert

end module unittest_type