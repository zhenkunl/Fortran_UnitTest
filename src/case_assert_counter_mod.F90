module case_assert_counter
  use string, only : string_t
  use counter_type, only : counter_t

  implicit none
  private
  public :: case_assert_counter_t

  type, extends(counter_t) :: case_assert_counter_t
    private
    type(string_t) :: name_
  contains
    private
    procedure, public, pass(self) :: init_name
    procedure, public, pass(self) :: header
    procedure, public, pass(self) :: footer
    procedure, public, pass(dtv)  :: write_message
  end type case_assert_counter_t

contains

  subroutine init_name(self, name)

    implicit none
    class(case_assert_counter_t), intent(inout) :: self
    type(string_t), intent(in)                  :: name

    self%name_ = name

  end subroutine init_name

  subroutine header(self)

    implicit none
    class(case_assert_counter_t), intent(in) :: self

    write(*, *) '|   +-> case ' // self%name_ // ':'

  end subroutine header

  subroutine footer(self)

    implicit none
    class(case_assert_counter_t), intent(in) :: self

    associate(unused => self)
    end associate

    write(*, *) '|   |'

  end subroutine footer

  subroutine write_message(dtv, unit, iotype, v_list, iostat, iomsg)

    implicit none
    class(case_assert_counter_t), intent(in) :: dtv       !< The string.
    integer, intent(in)                      :: unit      !< Logical unit.
    character(len=*), intent(in)             :: iotype    !< Edit descriptor.
    integer, intent(in)                      :: v_list(:) !< Edit descriptor list.
    integer, intent(out)                     :: iostat    !< IO status code.
    character(len=*), intent(inout)          :: iomsg     !< IO status message.
    type(string_t)                           :: message

    message = '|   |   |' // new_line('A') //                                             &
              ' |   +-> ' // dtv%name_ // ': ' // string_t(dtv%passed_num()) // ' of ' // &
              string_t(dtv%total_num()) // ' assertions succeed.'
    write(unit, '(dt)', iostat=iostat, iomsg=iomsg) message

  end subroutine write_message

end module case_assert_counter