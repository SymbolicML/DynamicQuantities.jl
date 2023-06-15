module Const

import ..@u_str
import ..@add_prefixes

# Source: http://physics.nist.gov/constants (2018)

# Exact, base:
"Speed of light in a vacuum. Standard."
const c = 299792458u"m/s"
"Planck constant. Standard."
const h = 6.62607015e-34u"J/Hz"
"Reduced Planck constant (h/2π). Standard."
const hbar = h / (2π)
"Elementary charge. Standard."
const e = 1.602176634e-19u"C"
"Boltzmann constant. Standard."
const k_B = 1.380649e-23u"J/K"
"Avogadro constant. Standard."
const N_A = 6.02214076e+23u"mol^-1"

# Exact, derived:
"Electron volt. Standard."
const eV = e * u"J/C"
@add_prefixes eV (m, k, M, G, T)
"Molar gas constant. Standard."
const R = N_A * k_B
"Faraday constant. Standard."
const F = N_A * e
"Stefan-Boltzmann constant. Standard."
const sigma_sb = (π^2/60) * k_B^4/(hbar^3 * c^2)

# Measured
"Fine-structure constant. Measured."
const alpha = 7.2973525693e-3
"Atomic mass unit (1/12th the mass of Carbon-12). Measured."
const u = 1.66053906660e-27u"kg"
"Newtonian constant of gravitation. Measured."
const G = 6.67430e-11u"m^3/(kg*s^2)"
"Vacuum magnetic permeability. Measured."
const mu_0 = 4π * alpha * hbar / (e^2 * c)
"Vacuum electric permittivity. Measured."
const eps_0 = 8.8541878128e-12u"F/m"
"Electron mass. Measured."
const m_e = 9.1093837015e-31u"kg"
"Proton mass. Measured."
const m_p = 1.67262192369e-27u"kg"
"Neutron mass. Measured."
const m_n = 1.67492749804e-27u"kg"
"Bohr radius. Measured."
const a_0 = hbar/(m_e * c * alpha)
"Coulomb constant (Note: SI units only!). Measured."
const k_e = 1/(4π * eps_0)
"Rydberg frequency. Measured."
const Ryd = alpha^2 * m_e * c^2 / (2 * h)


# Astro constants.
# Source: https://arxiv.org/abs/1510.07674

"Earth mass. Measured."
const M_earth = 5.97216787e+24u"kg"
"Solar mass. Measured."
const M_sun = 1.98840987e+30u"kg"
"Jupiter mass. Measured."
const M_jup = 1.8981246e+27u"kg"
"Nominal Earth equatorial radius. Standard."
const R_earth = 6.3781e+6u"m"
"Nominal Jupiter equatorial radius. Standard."
const R_jup = 7.1492e+7u"m"
"Nominal solar radius. Standard."
const R_sun = 6.957e+8u"m"
"Nominal solar luminosity. Standard."
const L_sun = 3.828e+26u"W"
"Standard luminosity at absolute bolometric magnitude 0. Standard."
const L_bol0 = 3.0128e+28u"W"
"Thomson scattering cross-section. Measured."
const sigma_T = 6.6524587321e-29u"m^2"
"Astronomical unit. Standard."
const au = 149597870700u"m"
"Parsec. Standard."
const pc = (648000/π) * au
@add_prefixes pc (k, M, G)
"Light year. Standard."
const ly = c * u"yr"
"Standard atmosphere. Standard."
const atm = 101325u"Pa"

end
