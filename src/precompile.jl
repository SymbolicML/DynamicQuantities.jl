import PrecompileTools

PrecompileTools.@setup_workload begin
    # Putting all the precompile statements inside the setup block
    # provides access to all imported modules and defined variables
    q1 = u"m"
    q2 = u"kg"
    q3 = u"m/s"

    PrecompileTools.@compile_workload begin
        # Basic unit construction with u_str macro
        u"m"
        
        # Basic operations
        q1 + q1
        q1 * 2.0
        q1 / 2.0
        q1 * q2
        q1 / q3
        
        # Common functions
        ustrip(q1)
        dimension(q1)
        
        # Conversion/display
        string(q1)
        print(devnull, q1)
        
        # Comparison operations
        q1 == q1
        
        # Array operations
        [q1, q1]
    end
end