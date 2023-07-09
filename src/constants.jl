module Constants

import ..Units as U

# Source: http://physics.nist.gov/constants (2018)

# Exact, base:
"Speed of light in a vacuum. Standard."
const c = 299792458 * U.m/U.s
"Planck constant. Standard."
const h = 6.62607015e-34 * U.J/U.Hz
"Reduced Planck constant (h/2π). Standard."
const hbar = h / (2π)
"Elementary charge. Standard."
const e = 1.602176634e-19 * U.C
"Boltzmann constant. Standard."
const k_B = 1.380649e-23 * U.J/U.K
"Avogadro constant. Standard."
const N_A = 6.02214076e+23 / U.mol

# Exact, derived:
"Electron volt. Standard."
const eV = e * U.J/U.C
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
const u = 1.66053906660e-27 * U.kg
"Newtonian constant of gravitation. Measured."
const G = 6.67430e-11 * U.m^3 / (U.kg * U.s^2)
"Vacuum magnetic permeability. Measured."
const mu_0 = 4π * alpha * hbar / (e^2 * c)
"Vacuum electric permittivity. Measured."
const eps_0 = 8.8541878128e-12 * U.F/U.m
"Electron mass. Measured."
const m_e = 9.1093837015e-31 * U.kg
"Proton mass. Measured."
const m_p = 1.67262192369e-27 * U.kg
"Neutron mass. Measured."
const m_n = 1.67492749804e-27 * U.kg
"Bohr radius. Measured."
const a_0 = hbar/(m_e * c * alpha)
"Coulomb constant (Note: SI units only!). Measured."
const k_e = 1/(4π * eps_0)
"Rydberg frequency. Measured."
const Ryd = alpha^2 * m_e * c^2 / (2 * h)


# Astro constants.
# Source: https://arxiv.org/abs/1510.07674

"Earth mass. Measured."
const M_earth = 5.97216787e+24 * U.kg
"Solar mass. Measured."
const M_sun = 1.98840987e+30 * U.kg
"Jupiter mass. Measured."
const M_jup = 1.8981246e+27 * U.kg
"Nominal Earth equatorial radius. Standard."
const R_earth = 6.3781e+6 * U.m
"Nominal Jupiter equatorial radius. Standard."
const R_jup = 7.1492e+7 * U.m
"Nominal solar radius. Standard."
const R_sun = 6.957e+8 * U.m
"Nominal solar luminosity. Standard."
const L_sun = 3.828e+26 * U.W
"Standard luminosity at absolute bolometric magnitude 0. Standard."
const L_bol0 = 3.0128e+28 * U.W
"Thomson scattering cross-section. Measured."
const sigma_T = 6.6524587321e-29 * U.m^2
"Astronomical unit. Standard."
const au = 149597870700 * U.m^2
"Parsec. Standard."
const pc = (648000/π) * au
@add_prefixes pc (k, M, G)
"Light year. Standard."
const ly = c * U.yr
"Standard atmosphere. Standard."
const atm = 101325 * U.Pa

end
