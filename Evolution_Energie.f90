module Evolution_Energie
  Use Data
  implicit none

contains

  Real Function Evol(Ep, sigma)

    Real, intent(in):: Ep, sigma
    Real:: frac, RT

    frac = dt/tau
    RT = R*T

    Evol = 1./(1.+2.*frac)*(Ep+(RT*frac)*(1.+sigma**2)+2.*sqrt(frac*RT*Ep*sigma))

  End Function Evol

End Module Evolution_Energie
