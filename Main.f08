! Author: Taylor Czerwinski
! Course: CSE 4250, Fall 2021
! Project: Proj1, Spread of Epidemics

! Newton's Approximation Function
REAL FUNCTION lambertW(z)
REAL, intent(in) :: z
INTEGER :: var
REAL :: y = 1

DO var = 1, 999, 1
y = y - (y*exp(y) - z)/(exp(y) + y*exp(y))
END DO
lambertW = y
END FUNCTION


PROGRAM Infected
IMPLICIT NONE
REAL :: lambertW
INTEGER:: var1, T, percent, case = 1
REAL:: n, alpha, gamma

DO
READ (*, *, IOSTAT = var1) n, alpha

IF (var1 < 0) THEN
EXIT
END IF
gamma = 1 + lambertW(-alpha*exp(-alpha))/alpha
T = NINT(gamma*n)
percent = INT((T/n)*100)
WRITE (*,*) "Case ", case, ":", T, percent, "%"
case = case + 1
END DO

END PROGRAM
