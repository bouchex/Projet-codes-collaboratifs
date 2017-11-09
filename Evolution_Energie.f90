module Evolution_Energie
  Use Data
  implicit none

contains

  Function Evol(Ep, sigma)
    Real, dimension(:) :: Ep, sigma
    Real, dimension(size(Ep)) :: Evol
    Real:: frac, RT
    Integer :: i

    frac = dt/tau
    RT = R*T
    Do i = 1, size(Ep)
      Evol(i) = 1./(1.+2.*frac)*(Ep(i)+(RT*frac)*(1.+sigma(i)**2)+2.*sqrt(frac*RT*Ep(i)*sigma(i)))
    End do
  End Function Evol

End Module Evolution_Energie
