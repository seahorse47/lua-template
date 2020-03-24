--- unit_tests.lua

local test = require "test_base"

local function runTestCase(testCaseName, ...)
    print(string.format("======== %s ========", testCaseName))
    test.runWith(...)
    print("")
end

local function emphasize(x)
    return string.format("<em>%s</em>", x)
end

local function percent(value, precision)
    local num = tonumber(value)
    -- return string.format("%.*f%%", precision or 0, num*100)
    if precision~=nil then
        local tmp = num>=0 and num or -num
        local a = 2
        while tmp >= 1 do
            a = a+1
            tmp = tmp/10
        end
        return string.format("%." .. (a+precision) .. "g%%", num*100)
    else
        -- return string.format("%g%%", num*100)
        return tostring(num*100).."%"
    end
end

runTestCase("variable replacing", [[Hello, ${name}]], {name = "world"})

runTestCase("indexing", [[Hello, ${names[i]}]], {names = {"C++", "Lua", "Java"}, i = 2})

runTestCase("filter", [[Hello, ${name|em}]], {name = "world"}, {em = emphasize})

runTestCase("filter with args", [[percentage with 2 decimals: ${value|percent<2>}]], {value = 0.85126}, {percent = percent})

runTestCase("set instruction", [[%{set name="~world~"}Hello, ${name}]])

runTestCase("if-else", [[%{if x != y}`x` is NOT equal to `y`.%{else}`x` is equal to `y`%{end if}]], {x = 1, y = 2})

local loopTmpl = [[start
%{for i=1,n,2}    i = ${i}
%{end for}end]]
runTestCase("for loop", loopTmpl, {n = 5})
