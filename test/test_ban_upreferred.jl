### These are tests we need to run with a fresh Julia runtime

import Unitful
import Unitful: @u_str
Unitful.preferunits(u"km")
using Test
import DynamicQuantities

x_unitful = 1.5u"km"
x_dq = DynamicQuantities.Quantity(1500.0, length=1)

@test_throws ErrorException convert(DynamicQuantities.Quantity, x_unitful)
@test_throws ErrorException convert(Unitful.Quantity, x_dq)
