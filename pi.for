C Fortran program for calculating Pi
C Using Gregory Leibniz Series
C Alex Kinch, April 18, 2026
      program picalc
      implicit none
                  double precision pi, x
                  double precision fact
                  integer q

                  pi = 0d0
                  x = 0d0
      do q = 0, 10, 1
                        x = x + ((((-1d0)**q) * fact(6*q) *
             &    (545140134d0*q + 13591409d0)) /
             &    (fact(3*q) * (fact(q))**3 *
             &    (640320d0**(3*q + 1.5d0))))
      end do
C      do q = 0, 1000000000, 1
C        pi4 = pi4 + sign * (1 / denom)
C        denom = denom + 2
C        sign = sign * (-1)
C      end do
C      pi = pi4 * 4
      pi = 1d0 / (12d0 * x)
      print *,'THE VALUE OF PI CALCULATED IS '
      print *,pi
      end program

C FUNCTION DECLARATION
      double precision function fact(x)
      implicit none
      integer,intent(in)::x
      fact = gamma(dble(x + 1))
      return
      end function fact