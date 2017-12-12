Module functions
Implicit none

Real, parameter :: pi=4.*atan(1.)

contains

  function NRJ_Aleatoires(N)
    Implicit None
    Integer, intent(IN) :: N
    Real, dimension(N) :: NRJ_Aleatoires
    Integer :: i
    Real, parameter :: y = 200000., x = 100000.

    Do i = 1, N
      NRJ_Aleatoires(i) = 0.
      NRJ_Aleatoires(i) = rand()*(y-x)+x
    End Do

    !write(6,*) NRJ_Aleatoires
  End function

  function Vect_Aleatoire(N)
    Implicit None
    Integer, intent(IN) :: N
    Real, dimension(N) :: Vect_Aleatoire
    Integer :: i
    Real :: s, m, u, v, Variance, Moyenne

    s = 1.
    m = 0.
    Variance = 0.
    Moyenne = 0.

    Do i = 1, N
      Vect_Aleatoire(i) = 0.
      u = rand()
      v = rand()
      Vect_Aleatoire(i) = m + s * Sqrt(-2 * Log(1-u)) * Cos(2 * pi * v)
      Moyenne = Moyenne + Vect_Aleatoire(i)
      Variance = Variance + Vect_Aleatoire(i)**2
    End Do

    !write(6,*) "Moyenne : ", Moyenne/N ," Variance : ", Variance/N

  End function

  function Temperature(Vect_NRJ,R)
    Implicit None
    Integer :: i
    Real, dimension(:), intent(IN) :: Vect_NRJ
    Real :: Temperature, R

    Temperature = 0.
    !write(6,*) "somme (moyenne) : ", sum(Vect_NRJ)/size(Vect_NRJ)
    Temperature = sum(Vect_NRJ)/(size(Vect_NRJ)*R)
    !write(6,*) "somme (temperature) : ", Temperature


  End function

End module
