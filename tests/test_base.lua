local Template = require "template"

local _M = {}

function _M.runWith(format, env,  filters, compileOptions)
    local tmpl = Template:new(format)

    local time1 = os.clock()
    tmpl:compile(compileOptions)
    local time2 = os.clock()

    local output = tmpl:render(env, filters)
    local time3 = os.clock()

    print(string.format("compile time: %.3f ms, render time: %.3f ms", (time2-time1)*1000, (time3-time2)*1000))
    print(string.format("output:\n%s", output))
end

return _M
