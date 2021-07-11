module shr_kind_mod
  use iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128

  implicit none
  private
  public :: i1, i2, i4, i8, r4, r8, r16

  integer, parameter :: i1  = int8    ! 1 byte integer
  integer, parameter :: i2  = int16   ! 2 byte integer
  integer, parameter :: i4  = int32   ! 4 byte integer
  integer, parameter :: i8  = int64   ! 8 byte integer
  integer, parameter :: r4  = real32  ! 4 byte real
  integer, parameter :: r8  = real64  ! 8 byte real
  integer, parameter :: r16 = real128 ! 16 byte real

end module shr_kind_mod