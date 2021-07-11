module string_type
  use shr_kind_mod

  implicit none
  private
  public :: string_t

  type :: string_t
    private
    character(len=:), allocatable  :: value_
  contains
    private
    generic, public                :: assignment(=) => assign_string_to_string, assign_character_to_string, &
                                      assign_string_to_character
    procedure, private, pass(lhs)  :: assign_string_to_string
    procedure, private, pass(lhs)  :: assign_character_to_string
    procedure, private, pass(rhs)  :: assign_string_to_character

    generic, public                :: operator(//) => string_concat_string, string_concat_character,        &
                                      character_concat_string
    procedure, private, pass(lhs)  :: string_concat_string
    procedure, private, pass(lhs)  :: string_concat_character
    procedure, private, pass(rhs)  :: character_concat_string

    generic, public                :: operator(==) => string_eq_string, string_eq_character,                &
                                      character_eq_string
    procedure, private, pass(lhs)  :: string_eq_string
    procedure, private, pass(lhs)  :: string_eq_character
    procedure, private, pass(rhs)  :: character_eq_string

    generic, public                :: operator(/=) => string_ne_string, string_ne_character,                &
                                      character_ne_string
    procedure, private, pass(lhs)  :: string_ne_string
    procedure, private, pass(lhs)  :: string_ne_character
    procedure, private, pass(rhs)  :: character_ne_string

    generic, public                :: operator(>) => string_gt_string, string_gt_character,                 &
                                      character_gt_string
    procedure, private, pass(lhs)  :: string_gt_string
    procedure, private, pass(lhs)  :: string_gt_character
    procedure, private, pass(rhs)  :: character_gt_string

    generic, public                :: operator(>=) => string_ge_string, string_ge_character,                &
                                      character_ge_string
    procedure, private, pass(lhs)  :: string_ge_string
    procedure, private, pass(lhs)  :: string_ge_character
    procedure, private, pass(rhs)  :: character_ge_string

    generic, public                :: operator(<) => string_lt_string, string_lt_character,                 &
                                      character_lt_string
    procedure, private, pass(lhs)  :: string_lt_string
    procedure, private, pass(lhs)  :: string_lt_character
    procedure, private, pass(rhs)  :: character_lt_string

    generic, public                :: operator(<=) => string_le_string, string_le_character,                &
                                      character_le_string
    procedure, private, pass(lhs)  :: string_le_string
    procedure, private, pass(lhs)  :: string_le_character
    procedure, private, pass(rhs)  :: character_le_string

    procedure, public, pass(self)  :: value    => get_string_value
    procedure, public, pass(self)  :: len      => len_string
    procedure, public, pass(self)  :: len_trim => len_trim_string
    procedure, public, pass(self)  :: trim     => trim_string
    procedure, public, pass(self)  :: adjustl  => adjustl_string
    procedure, public, pass(self)  :: adjustr  => adjustr_string
    procedure, public, pass(self)  :: reverse  => resverse_string
    procedure, public, pass(self)  :: to_lower => to_lower_string
    procedure, public, pass(self)  :: to_upper => to_upper_string
    procedure, public, pass(self)  :: capitalize
    procedure, public, pass(self)  :: colorize
    procedure, public, pass(self)  :: count    => count_substring
    procedure, public, pass(self)  :: index    => index_string
    procedure, public, pass(self)  :: repeat   => repeat_string
    procedure, public, pass(self)  :: scan     => scan_string
    procedure, public, pass(self)  :: verify   => verify_string
    procedure, public, pass(self)  :: at       => string_at
    procedure, public, pass(self)  :: start_with
    procedure, public, pass(self)  :: end_with

    generic, public                :: write(formatted)   => write_formatted
    procedure, private, pass(dtv)  :: write_formatted
    generic, public                :: write(unformatted) => write_unformatted
    procedure, private, pass(dtv)  :: write_unformatted

    final                          :: string_finalize
  end type string_t

  interface string_t
    module procedure constructor_from_int8
    module procedure constructor_from_int16
    module procedure constructor_from_int32
    module procedure constructor_from_int64
    module procedure constructor_from_real32
    module procedure constructor_from_real64
    module procedure constructor_from_logical
    module procedure constructor_from_character
  end interface string_t

contains

!------ constructor procedures start
  pure function constructor_from_int8(value) result(new)

    implicit none
    integer(i1), intent(in)       :: value
    type(string_t)                :: new
    character(len=range(value)+2) :: buffer

    write(buffer, '(i0)') value
    new = trim(buffer)

  end function constructor_from_int8

  pure function constructor_from_int16(value) result(new)

    implicit none
    integer(i2), intent(in)       :: value
    type(string_t)                :: new
    character(len=range(value)+2) :: buffer

    write(buffer, '(i0)') value
    new = trim(buffer)

  end function constructor_from_int16

  pure function constructor_from_int32(value) result(new)

    implicit none
    integer(i4), intent(in)       :: value
    type(string_t)                :: new
    character(len=range(value)+2) :: buffer

    write(buffer, '(i0)') value
    new = trim(buffer)

  end function constructor_from_int32

  pure function constructor_from_int64(value) result(new)

    implicit none
    integer(i8), intent(in)       :: value
    type(string_t)                :: new
    character(len=range(value)+2) :: buffer

    write(buffer, '(i0)') value
    new = trim(buffer)

  end function constructor_from_int64

  pure function constructor_from_real32(value, decimal_width, width) result(new)

    implicit none
    real(r4), intent(in)          :: value
    integer, intent(in), optional :: decimal_width
    integer, intent(in), optional :: width
    type(string_t)                :: new
    character(len=range(value)+2) :: buffer
    character(len=5)              :: format
    integer                       :: total_width

    if (present(decimal_width)) then
      if (present(width)) then
        total_width = max(width, decimal_width + 7)
      else
        total_width = decimal_width + 7
      end if
      write(format, '(a1, i0, a1, i0)') 'G', total_width, '.', decimal_width
      write(buffer, '(' // format // ')') value
      new = trim(adjustl(buffer))
    else
      write(buffer, *) value
      new = trim(adjustl(buffer))
    end if

  end function constructor_from_real32

  pure function constructor_from_real64(value, decimal_width, width) result(new)

    implicit none
    real(r8), intent(in)          :: value
    integer, intent(in), optional :: decimal_width
    integer, intent(in), optional :: width
    type(string_t)                :: new
    character(len=range(value)+2) :: buffer
    character(len=5)              :: format
    integer                       :: total_width

    if (present(decimal_width)) then
      if (present(width)) then
        total_width = max(width, decimal_width + 7)
      else
        total_width = decimal_width + 7
      end if
      write(format, '(a1, i0, a1, i0)') 'G', total_width, '.', decimal_width
      write(buffer, '(' // format // ')') value
      new = trim(adjustl(buffer))
    else
      write(buffer, *) value
      new = trim(adjustl(buffer))
    end if

  end function constructor_from_real64

  pure function constructor_from_logical(value) result(new)

    implicit none
    logical, intent(in) :: value
    type(string_t)      :: new

    if (value) then
      new = "True"
    else
      new = "False"
    end if

  end function constructor_from_logical

  pure function constructor_from_character(value) result(new)

    implicit none
    character(len=*), intent(in) :: value
    type(string_t)               :: new

    new = value

  end function constructor_from_character
!------ constructor procedures end

  pure subroutine string_finalize(self)

    implicit none
    type(string_t), intent(inout) :: self

    if (allocated(self%value_)) then
      deallocate(self%value_)
    end if

  end subroutine string_finalize

!------ assignment(=) procedures start
  pure subroutine assign_string_to_string(lhs, rhs)

    implicit none
    class(string_t), intent(inout) :: lhs
    type(string_t), intent(in)     :: rhs

    lhs%value_ = rhs%value_

  end subroutine assign_string_to_string

  pure subroutine assign_character_to_string(lhs, rhs)

    implicit none
    class(string_t), intent(inout) :: lhs
    character(len=*), intent(in) :: rhs

    lhs%value_ = rhs

  end subroutine assign_character_to_string

  pure subroutine assign_string_to_character(lhs, rhs)

    implicit none
    character(len=:), allocatable, intent(out) :: lhs
    class(string_t), intent(in)                :: rhs

    lhs = rhs%value_

  end subroutine assign_string_to_character
!------ assignment(=) procedures end

!------ operator(//) procedures start
  pure function string_concat_string(lhs, rhs) result(concat_string)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    type(string_t)              :: concat_string

    concat_string = lhs%value_//rhs%value_

  end function string_concat_string

  pure function string_concat_character(lhs, rhs) result(concat_string)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    type(string_t)               :: concat_string

    concat_string = lhs%value_//rhs

  end function string_concat_character

  pure function character_concat_string(lhs, rhs) result(concat_string)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    type(string_t)               :: concat_string

    concat_string = lhs//rhs%value_

  end function character_concat_string
!------ operator(//) procedures end

!------ operator(==) procedures start
  pure function string_eq_string(lhs, rhs) result(equals)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    logical                     :: equals

    equals = lhs%value_ == rhs%value_

  end function string_eq_string

  pure function string_eq_character(lhs, rhs) result(equals)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equals

    equals = lhs%value_ == rhs

  end function string_eq_character

  pure function character_eq_string(lhs, rhs) result(equals)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    logical                      :: equals

    equals = lhs == rhs%value_

  end function character_eq_string
!------ operator(==) procedures end

!------ operator(/=) procedures start
  pure function string_ne_string(lhs, rhs) result(equals)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    logical                     :: equals

    equals = lhs%value_ /= rhs%value_

  end function string_ne_string

  pure function string_ne_character(lhs, rhs) result(equals)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equals

    equals = lhs%value_ /= rhs

  end function string_ne_character

  pure function character_ne_string(lhs, rhs) result(equals)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    logical                      :: equals

    equals = lhs /= rhs%value_

  end function character_ne_string
!------ operator(/=) procedures end

!------ operator(>) procedures start
  pure function string_gt_string(lhs, rhs) result(equals)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    logical                     :: equals

    equals = lhs%value_ > rhs%value_

  end function string_gt_string

  pure function string_gt_character(lhs, rhs) result(equals)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equals

    equals = lhs%value_ > rhs

  end function string_gt_character

  pure function character_gt_string(lhs, rhs) result(equals)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    logical                      :: equals

    equals = lhs > rhs%value_

  end function character_gt_string
!------ operator(>) procedures end

!------ operator(>=) procedures start
  pure function string_ge_string(lhs, rhs) result(equals)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    logical                     :: equals

    equals = lhs%value_ >= rhs%value_

  end function string_ge_string

  pure function string_ge_character(lhs, rhs) result(equals)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equals

    equals = lhs%value_ >= rhs

  end function string_ge_character

  pure function character_ge_string(lhs, rhs) result(equals)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    logical                      :: equals

    equals = lhs >= rhs%value_

  end function character_ge_string
!------ operator(>=) procedures end

!------ operator(<) procedures start
  pure function string_lt_string(lhs, rhs) result(equals)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    logical                     :: equals

    equals = lhs%value_ < rhs%value_

  end function string_lt_string

  pure function string_lt_character(lhs, rhs) result(equals)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equals

    equals = lhs%value_ < rhs

  end function string_lt_character

  pure function character_lt_string(lhs, rhs) result(equals)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    logical                      :: equals

    equals = lhs < rhs%value_

  end function character_lt_string
!------ operator(<) procedures end

!------ operator(<=) procedures start
  pure function string_le_string(lhs, rhs) result(equals)

    implicit none
    class(string_t), intent(in) :: lhs
    type(string_t), intent(in)  :: rhs
    logical                     :: equals

    equals = lhs%value_ <= rhs%value_

  end function string_le_string

  pure function string_le_character(lhs, rhs) result(equals)

    implicit none
    class(string_t), intent(in)  :: lhs
    character(len=*), intent(in) :: rhs
    logical                      :: equals

    equals = lhs%value_ <= rhs

  end function string_le_character

  pure function character_le_string(lhs, rhs) result(equals)

    implicit none
    character(len=*), intent(in) :: lhs
    class(string_t), intent(in)  :: rhs
    logical                      :: equals

    equals = lhs <= rhs%value_

  end function character_le_string
!------ operator(<=) procedures end

  pure function get_string_value(self) result(string_value)

    implicit none
    class(string_t), intent(in)   :: self
    character(len=:), allocatable :: string_value

    string_value = self%value_

  end function get_string_value

  pure function len_string(self) result(length)

    implicit none
    class(string_t), intent(in) :: self
    integer                     :: length

    length = len(self%value_)

  end function len_string

  pure function len_trim_string(self) result(length)

    implicit none
    class(string_t), intent(in) :: self
    integer                     :: length

    length = len_trim(self%value_)

  end function len_trim_string

  pure function trim_string(self) result(trimmed_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: trimmed_string

    trimmed_string = trim(self%value_)

  end function trim_string

  pure function adjustl_string(self) result(adjusted_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: adjusted_string

    adjusted_string = adjustl(self%value_)

  end function adjustl_string

  pure function adjustr_string(self) result(adjusted_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: adjusted_string

    adjusted_string = adjustr(self%value_)

  end function adjustr_string

  pure function resverse_string(self) result(reversed_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: reversed_string
    integer                     :: i, n

    reversed_string = self
    n = self%len()
    do i = 1, n
      reversed_string%value_(n-i+1:n-i+1) = self%value_(i:i)
    end do

  end function resverse_string

  pure function to_lower_string(self) result(lower_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: lower_string
    integer                     :: i

    lower_string = self
    do i = 1, self%len()
      select case (lower_string%value_(i:i))
      case ('A':'Z')
        lower_string%value_(i:i) = char(iachar(lower_string%value_(i:i))+32)
      case default
      end select
    end do

  end function to_lower_string

  pure function to_upper_string(self) result(upper_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: upper_string
    integer                     :: i

    upper_string = self
    do i = 1, self%len()
      select case (upper_string%value_(i:i))
      case ('a':'z')
        upper_string%value_(i:i) = achar(iachar(upper_string%value_(i:i))-32)
      case default
      end select
    end do

  end function to_upper_string

  pure function capitalize(self) result(capitalized_string)

    implicit none
    class(string_t), intent(in) :: self
    type(string_t)              :: capitalized_string

    capitalized_string = self%to_lower()
    select case (capitalized_string%value_(1:1))
    case ('a':'z')
      capitalized_string%value_(1:1) = achar(iachar(capitalized_string%value_(1:1))-32)
    case default
    end select

  end function capitalize

  pure function colorize(self, color) result(colorized_string)

    implicit none
    class(string_t), intent(in)            :: self
    character(len=*), intent(in), optional :: color
    type(string_t)                         :: colorized_string
    type(string_t)                         :: color_string

    colorized_string = self
    if (present(color)) then
      color_string = color
      color_string = color_string%to_lower()
      select case (color_string%value_)
      case ('red')
        colorized_string = char(27) // "[31m" // self // char(27) // "[0m"
      case ('green')
        colorized_string = char(27) // "[32m" // self // char(27) // "[0m"
      case ('yellow')
        colorized_string = char(27) // "[33m" // self // char(27) // "[0m"
      case ('blue')
        colorized_string = char(27) // "[34m" // self // char(27) // "[0m"
      case default
      end select
    end if

  end function colorize

  pure function count_substring(self, substring) result(number)

    implicit none
    class(string_t), intent(in)  :: self
    character(len=*), intent(in) :: substring
    integer                      :: number
    integer                      :: start, idx

    number = 0
    if (len(substring) < self%len()) then
      start = 1
      do
        idx = index(self%value_(start:), substring)
        if (idx == 0) then
          exit
        else
          number = number + 1
        end if
        start = start + idx + len(substring) - 1
      end do
    end if

  end function count_substring

  pure function index_string(self, substring, back) result(idx)

    implicit none
    class(string_t), intent(in)   :: self
    character(len=*), intent(in)  :: substring
    logical, intent(in), optional :: back
    integer                       :: idx

    idx = index(self%value_, substring, back)

  end function index_string

  pure function repeat_string(self, ncopies) result(repeated_string)

    implicit none
    class(string_t), intent(in) :: self
    integer, intent(in)         :: ncopies
    type(string_t)              :: repeated_string

    repeated_string = repeat(self%value_, ncopies)

  end function repeat_string

  pure function scan_string(self, set, back) result(idx)

    implicit none
    class(string_t), intent(in)   :: self
    character(len=*), intent(in)  :: set
    logical, intent(in), optional :: back
    integer                       :: idx

    idx = scan(self%value_, set, back)

  end function scan_string

  pure function verify_string(self, set, back) result(idx)

    implicit none
    class(string_t), intent(in)   :: self
    character(len=*), intent(in)  :: set
    logical, intent(in), optional :: back
    integer                       :: idx

    idx = verify(self%value_, set, back)

  end function verify_string

  pure function string_at(self, start, end) result(sliced_string)

    implicit none
    class(string_t), intent(in)   :: self
    integer, intent(in)           :: start
    integer, intent(in), optional :: end
    type(string_t)                :: sliced_string

    if (present(end)) then
      sliced_string = self%value_(start:end)
    else
      sliced_string = self%value_(start:start)
    end if

  end function string_at

  pure function start_with(self, prefix, start, end) result(res)

    implicit none
    class(string_t), intent(in)   :: self
    character(len=*), intent(in)  :: prefix
    integer, intent(in), optional :: start
    integer, intent(in), optional :: end
    logical                       :: res
    integer                       :: start_, end_

    if (present(start)) then
      start_ = start
    else
      start_ = 1
    end if

    if (present(end)) then
      end_ = end
    else
      end_ = self%len()
    end if

    res = index(self%value_(start_:end_), prefix) == 1

  end function start_with

  pure function end_with(self, suffix, start, end) result(res)

    implicit none
    class(string_t), intent(in)   :: self
    character(len=*), intent(in)  :: suffix
    integer, intent(in), optional :: start
    integer, intent(in), optional :: end
    logical                       :: res
    integer                       :: start_, end_

    if (present(start)) then
      start_ = start
    else
      start_ = 1
    end if

    if (present(end)) then
      end_ = end
    else
      end_ = self%len()
    end if

    res = self%value_(end_-len(suffix)+1:end_) == suffix

  end function end_with

  subroutine write_formatted(dtv, unit, iotype, v_list, iostat, iomsg)

    implicit none
    class(string_t), intent(in)     :: dtv       !< The string.
    integer, intent(in)             :: unit      !< Logical unit.
    character(len=*), intent(in)    :: iotype    !< Edit descriptor.
    integer, intent(in)             :: v_list(:) !< Edit descriptor list.
    integer, intent(out)            :: iostat    !< IO status code.
    character(len=*), intent(inout) :: iomsg     !< IO status message.

    if (allocated(dtv%value_)) then
      write(unit, '(a)', iostat=iostat, iomsg=iomsg) dtv%value_
    else
      write(unit, '(a)', iostat=iostat, iomsg=iomsg) ''
    end if

  end subroutine write_formatted

  subroutine write_unformatted(dtv, unit, iostat, iomsg)
  
  implicit none
  class(string_t), intent(in)     :: dtv    !< The string.
  integer, intent(in)             :: unit   !< Logical unit.
  integer, intent(out)            :: iostat !< IO status code.
  character(len=*), intent(inout) :: iomsg  !< IO status message.

  if (allocated(dtv%value_)) then
    write(unit, iostat=iostat, iomsg=iomsg) dtv%value_
  else
    write(unit, iostat=iostat, iomsg=iomsg) ''
  end if

  end subroutine write_unformatted

end module string_type