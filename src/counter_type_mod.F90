module counter_type

  implicit none
  private
  public :: counter_t

  type, abstract :: counter_t
    private
    integer :: passed_num_
    integer :: failed_num_
    integer :: total_num_
  contains
    private
    procedure, public, pass(self)                           :: init
    procedure, public, pass(self)                           :: passed_num
    procedure, public, pass(self)                           :: failed_num
    procedure, public, pass(self)                           :: total_num
    procedure, public, pass(self)                           :: add_passed
    procedure, public, pass(self)                           :: add_failed
    procedure, public, pass(self)                           :: add_total
    generic, public                                         :: write(formatted) => write_message
    procedure(write_message_i), public, pass(dtv), deferred :: write_message
  end type counter_t

  abstract interface
    subroutine write_message_i(dtv, unit, iotype, v_list, iostat, iomsg)

      import counter_t
      implicit none
      class(counter_t), intent(in)    :: dtv
      integer, intent(in)             :: unit      !< Logical unit.
      character(len=*), intent(in)    :: iotype    !< Edit descriptor.
      integer, intent(in)             :: v_list(:) !< Edit descriptor list.
      integer, intent(out)            :: iostat    !< IO status code.
      character(len=*), intent(inout) :: iomsg     !< IO status message.

    end subroutine write_message_i
  end interface

contains

  subroutine init(self)

    implicit none
    class(counter_t), intent(inout) :: self

    self%passed_num_ = 0
    self%failed_num_ = 0
    self%total_num_  = 0

  end subroutine init

  function passed_num(self) result(num)

    implicit none
    class(counter_t), intent(in) :: self
    integer                      :: num

    num = self%passed_num_
  
  end function passed_num

  function failed_num(self) result(num)

    implicit none
    class(counter_t), intent(in) :: self
    integer                      :: num

    num = self%failed_num_
  
  end function failed_num

  function total_num(self) result(num)

    implicit none
    class(counter_t), intent(in) :: self
    integer                      :: num

    num = self%total_num_
  
  end function total_num

  subroutine add_passed(self)

    implicit none
    class(counter_t), intent(inout) :: self

    self%passed_num_ = self%passed_num_ + 1

  end subroutine add_passed

  subroutine add_failed(self)

    implicit none
    class(counter_t), intent(inout) :: self

    self%failed_num_ = self%failed_num_ + 1

  end subroutine add_failed

  subroutine add_total(self)

    implicit none
    class(counter_t), intent(inout) :: self

    self%total_num_  = self%total_num_ + 1

  end subroutine add_total

end module counter_type