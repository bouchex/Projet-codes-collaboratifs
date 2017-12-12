module Evolution_Energie
  Use Data
  Use functions
  implicit none

contains

  Function Evol(Ep)
    Real, dimension(:) :: Ep
    Real, dimension(size(Ep)) :: Evol, sigma
    Real:: frac, RT
    Integer :: i

    frac = dt/tau
    RT = R*T
    sigma = Vect_Aleatoire(size(Ep))
    Do i = 1, size(Ep)
      !write(6,*) "Evol : ", Ep(i), " sigma : ", sigma(i)
      Evol(i) = 1./(1.+2.*frac)*(Ep(i)+(RT*frac)*(1.+sigma(i)**2)+2.*sqrt(frac*RT*Ep(i))*sigma(i))
      !write(6,*) "Evol : ", Evol(i)
    End do
  End Function Evol

End Module Evolution_Energie
