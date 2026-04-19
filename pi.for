C Fortran program for calculating Pi
C Using Chudnovsky Series
C Alex Kinch, April 18, 2026
      program picalc
      implicit none
                  real(kind=16) pi, x
                  real(kind=16) fact
                  external fact
                  integer q

                  pi = 0.0_16
                  x = 0.0_16
      do q = 0, 10, 1
                        x = x + ((((-1.0_16)**q) * fact(6*q) *
     1    (545140134.0_16*q + 13591409.0_16)) /
     1    (fact(3*q) * (fact(q))**3 *
     1    (640320.0_16**(3*q + 1.5_16))))
      end do
C      do q = 0, 1000000000, 1
C        pi4 = pi4 + sign * (1 / denom)
C        denom = denom + 2
C        sign = sign * (-1)
C      end do
C      pi = pi4 * 4
      pi = 1.0_16 / (12.0_16 * x)
      print *,'THE VALUE OF PI CALCULATED IS '
      write(*,'(F40.35)') pi
      end program

C FUNCTION DECLARATION
      real(kind=16) function fact(x)
      implicit none
      integer,intent(in)::x
      fact = gamma(real(x + 1,kind=16))
      return
      end function fact
