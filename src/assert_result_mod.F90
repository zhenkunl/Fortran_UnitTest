module assert_result
  use string, only : string_t

  implicit none
  private
  public :: assert_result_t

  type :: assert_result_t
    private
    integer        :: id_
    type(string_t) :: operator_
    type(string_t) :: left_operand_
    type(string_t) :: right_operand_
    logical        :: passed_
    type(string_t) :: file_name_
    integer        :: line_number_
  contains
    private
    procedure, private, pass(self) :: passed_message
    procedure, private, pass(self) :: failed_message
    generic, public                :: write(formatted) => write_message
    procedure, private, pass(dtv)  :: write_message
  end type assert_result_t

  interface assert_result_t
    procedure constructor
  end interface assert_result_t

contains

  function constructor(id, left_operand, operator, right_operand, passed, file_name, line_number) result(assert_result)

    implicit none
    integer, intent(in)                    :: id
    type(string_t), intent(in)             :: left_operand
    type(string_t), intent(in)             :: operator
    type(string_t), intent(in)             :: right_operand
    logical, intent(in)                    :: passed
    character(len=*), intent(in), optional :: file_name
    integer, intent(in), optional          :: line_number
    type(assert_result_t)                  :: assert_result
    type(string_t)                         :: file_name_
    integer                                :: line_number_

    if (present(file_name)) then
      file_name_ = file_name
    else
      file_name_ = 'Unknown file'
    end if
    if (present(line_number)) then
      line_number_ = line_number
    else
      line_number_ = -1
    end if

    assert_result%id_            = id
    assert_result%operator_      = operator
    assert_result%left_operand_  = left_operand
    assert_result%right_operand_ = right_operand
    assert_result%passed_        = passed
    assert_result%file_name_     = file_name_
    assert_result%line_number_   = line_number_

  end function constructor

  function passed_message(self) result(message)

    implicit none
    class(assert_result_t), intent(in) :: self
    type(string_t)                     :: message

    message = '|   |   |' // new_line('A')  // &
              ' |   |   +-> Assertion #' // string_t(self%id_) // ' succeed!'

  end function passed_message

  function failed_message(self) result(message)

    implicit none
    class(assert_result_t), intent(in) :: self
    type(string_t)                     :: message

    if (self%right_operand_ /= '') then
      message = '|   |   |' // new_line('A') //                                                     &
                ' |   |   +-> Assertion #' // string_t(self%id_) // ' failed with reason:' //       &
                ' expected (' // self%left_operand_ // ') ' // self%operator_ //                    &
                ' actual (' // self%right_operand_ // ')' // new_line('A') //                       &
                ' |   |   +-> Check line: ' // self%file_name_ // ':' // string_t(self%line_number_)
    else
      message = '|   |   |' // new_line('A') //                                                     &
                ' |   |   +-> Assertion #' // string_t(self%id_) // ' failed with reason:' //       &
                ' expected (' // self%left_operand_ // ') ' // 'is ' // self%operator_ //           &
                new_line('A') //                                                                    &
                ' |   |   +-> Check line: ' // self%file_name_ // ':' // string_t(self%line_number_)
    end if

  end function failed_message

  subroutine write_message(dtv, unit, iotype, v_list, iostat, iomsg)

    implicit none
    class(assert_result_t), intent(in) :: dtv       !< The string.
    integer, intent(in)                :: unit      !< Logical unit.
    character(len=*), intent(in)       :: iotype    !< Edit descriptor.
    integer, intent(in)                :: v_list(:) !< Edit descriptor list.
    integer, intent(out)               :: iostat    !< IO status code.
    character(len=*), intent(inout)    :: iomsg     !< IO status message.
    type(string_t)                     :: message

    if (dtv%passed_) then
      message = dtv%passed_message()
    else
      message = dtv%failed_message()
    end if
    write(unit, '(dt)', iostat=iostat, iomsg=iomsg) message

  end subroutine write_message

end module assert_result