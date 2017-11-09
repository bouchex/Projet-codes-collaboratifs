Module functions
Implicit none

Real, parameter :: pi=4.*atan(1.)

contains

  function NRJ_Aleatoires(N)
    Implicit None
    Integer, intent(IN) :: N
    Real, dimension(N), intent(OUT) :: NRJ_Aleatoires
    Integer :: i
    Real, parameter :: y = 200000., x = 100000.

    Do i = 1, N
      NRJ_Aleatoires(i) = rand(0)*(y-x)+x
    End Do

    !write(6,*) NRJ_Aleatoires
  End function

  function sigma()
    Implicit none
    Real, intent(OUT) :: sigma
    Real :: u, v, s, m

    s = 1.
    m = 0.
    u = rand(0)
    v = rand(0)
    sigma = m + s * Sqrt(-2 * Log(1-u)) * Cos(2 * pi * v)

  End function

End module
