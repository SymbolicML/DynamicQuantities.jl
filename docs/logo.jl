using Luxor
using Luxor.Colors

image_width = 500

@svg begin
    circle_radius = 0.85 * image_width / 2
    scale(circle_radius, circle_radius)

    offsets = [-0.13, 0.06, -0.07]
    colors = Base.splat(RGB).([Luxor.julia_green, Luxor.julia_red, Luxor.julia_purple])
    strip_thickness = 0.25
    stripgap = 0.04
    sectorwidth = 0.25
    sector_whiten = 0.2

    prefixes = ["p", "f", "μ", "m", "", "k"]
    units = ["g", "s", "m"]

    whiten(c, fraction) = Colors.weighted_color_mean(fraction, c, colorant"white")

    n = length(units)

    full_height = n * stripgap + n * strip_thickness

    sethue(Luxor.julia_blue)
    box(O, sectorwidth * 0.75, full_height + 0.15, action = :fill)

    for i in 1:3
        color = colors[i]

        y = (i-1) * stripgap + (i-1) * strip_thickness - (full_height * (n-1) / n / 2) #+ (.5 * strip_thickness)

        for (j, prefix) in enumerate(prefixes)
            x = offsets[i] + (j * sectorwidth) - ((length(prefixes)+1)/2 * sectorwidth)
            
            # sethue(colors[i])
            sethue(iseven(j) ? colors[i] : whiten(colors[i], 1 - sector_whiten))

            box(Point(x, y), sectorwidth, strip_thickness, action = :fill)
            
            sethue("white")
            s = "$(prefix)$(units[i])"

            @layer begin
                setfont("Helvetica Bold", 20)
                settext(s, Point(x, y); valign = "center", halign = "center", markup = true)
            end
        end
    end

    sethue(Luxor.julia_blue)
    setline(20)
    box(O, sectorwidth + 0.08, full_height + 0.15, action = :stroke)

end image_width image_width