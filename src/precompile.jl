import PrecompileTools

PrecompileTools.@setup_workload begin
    PrecompileTools.@compile_workload begin
        q1 = u"m"
        q1_s = us"m"
        q2 = u"kg"
        q2_s = us"kg"
        q3 = u"m/s"
        q3_s = us"m/s"
        
        # Basic operations
        q1 + q1
        q1_s + q1_s
        q1 + q1_s
        q1_s + q1

        q1 - q1
        q1_s - q1_s
        q1 - q1_s
        q1_s - q1

        q1 * 2.0
        q1_s * 2.0
        2.0 * q1
        q1_s * 2.0

        q1 / 2.0
        q1_s / 2.0

        2.0 / q1
        2.0 / q1_s

        q1 * q2
        q1_s * q2_s
        q1 * q2_s
        q1_s * q2

        q1 / q3
        q1_s / q3_s
        q1 / q3_s
        q1_s / q3

        # Power operations
        q3^2
        q3_s^2
        q3^2.0
        q3_s^2.0
        inv(q3)
        inv(q3_s)

        # Conversion/display
        string(q3)
        string(q3_s)
        show(devnull, MIME"text/plain"(), q1)
        show(devnull, MIME"text/plain"(), q1_s)
        print(devnull, q1)
        print(devnull, q1_s)

        # Comparison operations
        q1 == q1
        q1_s == q1_s
        q1 == q1_s
        q1_s == q1

        # Array operations
        x = fill(q1, 10)
        y = fill(q1_s, 10)
        print(devnull, x)
        print(devnull, y)
    end
end
