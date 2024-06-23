      program bisectionprog
      implicit none
      real:: f,epsilon,up,lo,n,m,eA,vr,vrold,fdm,fdl,fdu
C ALLOWED ERROR
      epsilon = 0.0001*100
C UPPER AND LOWER BOUNDS
      up = 0.8
      lo = 0.7
C OTHER DECLARATIONS
      n = 1
      m = 1
C WHILE TRUE LOOP
      do while (.TRUE.)
C      do while (n < 10)
C BISECTION
        vr = (up + lo) / 2
C IF LOOP
        if (n > 1) then
          eA = abs((vr-vrold)/vr)*100
          if (eA < epsilon) then
C BREAKS WHEN ERROR IS LOWER THAN DESIRED
            exit
          endif
        endif
C FIND VALUE AT MIDPT
        fdm = f(vr)
C VALUE AT LOWER BOUND
        fdl = f(lo)
C VALUE AT UPPER BOUND
        fdu = f(up)
C IF CROSSES X AXIS IN LOWER HALF, MAKE MIDPT UPPER BOUND
        if (fdm*fdl .LT. 0) then
          up = vr
        else
          lo = vr
        endif
C OLD ROOT VARIABLE
        vrold = vr
        print *,'ITERATION #',n,'\n'
        print *,'LOW BOUND: ',lo,' LOW VALUE: ',fdl
        print *,'MID POINT: ',vr,' MID VALUE: ',fdm
        print *,'UPR BOUND: ',up,' UPR VALUE: ',fdu
C ITERATE N BY ONE
        n = n + 1
      enddo
      print *,'THE RESULTING ROOT IS AT '
      print *,vr
      print *,' WITH A VALUE OF '
      print *,fdm
      end program
C FUNCTION DECLARATION
      real function f(x)
      implicit none
      real,intent(in)::x
      f = (5 - x)/200 - (10**(-15))*(exp(x/0.025) - 1)
      return
      end function f

