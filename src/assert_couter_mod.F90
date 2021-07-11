module assert_counter
  use string_type, only : string_t
  use counter_type, only : counter_t

  implicit none
  private
  public :: assert_counter_t

  type, extends(counter_t) :: assert_counter_t
  contains
    private
    procedure, public, pass(dtv)  :: write_message
  end type assert_counter_t

contains

  subroutine write_message(dtv, unit, iotype, v_list, iostat, iomsg)

    implicit none
    class(assert_counter_t), intent(in) :: dtv       !< The string.
    integer, intent(in)                 :: unit      !< Logical unit.
    character(len=*), intent(in)        :: iotype    !< Edit descriptor.
    integer, intent(in)                 :: v_list(:) !< Edit descriptor list.
    integer, intent(out)                :: iostat    !< IO status code.
    character(len=*), intent(inout)     :: iomsg     !< IO status message.
    type(string_t)                      :: message

    message = string_t(dtv%passed_num()) // ' of ' // string_t(dtv%total_num()) // ' assertions succeed.'
    write(unit, '(a)', iostat=iostat, iomsg=iomsg) message%value()

  end subroutine write_message

end module assert_counter