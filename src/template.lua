--[[
    Filename:    template.lua
    Author:      chenhailong
    Datetime:    2020-03-22 12:35:39
    Description: See below.
--]]

local kTokenizerInitState = "INITIAL"

--- @class TokenRules
local TokenRules = {}

function TokenRules:new(...)
    local obj = setmetatable({}, { __index = self, })
    obj:initialize(...)
    return obj
end

function TokenRules:initialize(config)
    self._rulesForStates = self:_preprocessRules(config)
    self._errorCallback = config.errorCallback
    self._tokEOF = config.EOF
end

function TokenRules:hasState(state)
    return self._rulesForStates[state]~=nil
end

function TokenRules:EOF()
    return self._tokEOF
end

function TokenRules:matchRule(state, source, pos)
    local rules = self._rulesForStates[state]
    assert(rules~=nil)
    for _, rule in ipairs(rules) do
        if rule.length then
            -- plain match
            local s, e = pos, pos+rule.length-1
            if source:sub(s, e)==rule.pattern then
                return rule, s, e, rule.pattern, rule.pattern
            end
        else
            local s, e, src, text = source:find(rule.pattern, pos)
            if s~=nil then
                return rule, s, e, src, text
            end
        end
    end
end

function TokenRules:getErrorCallback()
    return self._errorCallback
end

function TokenRules:_preprocessRules(config)
    local rulesForStates = {
        [kTokenizerInitState] = {
            id = kTokenizerInitState,
        },
    }
    if config.states~=nil then
        for _, state in ipairs(config.states) do
            assert(state.id~=nil, "Missing state id")
            assert(rulesForStates[state.id]==nil, string.format("Duplicated state id: %s", state.id))
            rulesForStates[state.id] = {
                id = state.id,
                exclusive = state.exclusive,
            }
        end
    end
    assert(config.tokens~=nil)
    for _, tokRule in ipairs(config.tokens) do
        local states = tokRule.states
        local rule = {
            skip = tokRule.skip,
            callback = tokRule.callback,
        }
        if tokRule.regex~=nil then
            rule.type = tokRule.type
            rule.pattern = "^(" .. tokRule.regex .. ")"
            rule.length = nil
        elseif tokRule.literal~=nil then
            rule.type = tokRule.type or tokRule.literal
            rule.pattern = tokRule.literal
            rule.length = tokRule.literal:len()
        end
        assert(rule.type~=nil, "Rule type is missing")
        if states==nil then
            for id, statedRules in pairs(rulesForStates) do
                if not statedRules.exclusive then
                    statedRules[#statedRules + 1] = rule
                end
            end
        else
            for _, state in pairs(states) do
                local statedRules = rulesForStates[state]
                assert(statedRules~=nil, string.format("Unkown state: %s", state))
                statedRules[#statedRules + 1] = rule
            end
        end
    end
    return rulesForStates
end


--- @class Tokenizer
local Tokenizer = {}

function Tokenizer:new(...)
    local obj = setmetatable({}, { __index = self, })
    obj:initialize(...)
    return obj
end

function Tokenizer:initialize(rules)
    self._rules = rules
    self._source = nil
    self._len = 0
    self._pos = 1
    self._state = kTokenizerInitState
    self._aheads = {}
end

function Tokenizer:setSource(source)
    self._source = source
    self._len = source:len()
    self._pos = 1
    self._marks = {}
end

function Tokenizer:getSource()
    return self._source
end

function Tokenizer:resetPosition(pos)
    self._pos = pos
end

function Tokenizer:currentPosition()
    return self._pos
end

function Tokenizer:changeState(state)
    if state==nil then
        state = kTokenizerInitState
    end
    if state==self._state then
        return
    end
    assert(self._rules:hasState(state), string.format("Invalid target state: %s", state))
    self._state = state
    self._aheads.next = nil
end

function Tokenizer:currentState()
    return self._state
end

function Tokenizer:mark(label)
    local marks = self._marks
    if marks==nil then
        return
    end
    marks[#marks + 1] = {
        label = label,
        pos = self._pos,
        state = self._state,
    }
end

function Tokenizer:firstMark(label, from)
    local marks = self._marks
    if marks~=nil then
        local total = #marks
        if from==nil then from = 1 end
        if from<0 then from = total + 1 + from end
        if from<0 then from = 1 end
        for i = from, #marks, 1 do
            local mark = marks[i]
            if label==nil or label==mark.label then
                return i, mark
            end
        end
    end
    return nil, nil
end

function Tokenizer:lastMark(label, from)
    local marks = self._marks
    if marks~=nil then
        local total = #marks
        if from==nil or from>total then from = total end
        if from<0 then from = total + 1 + from end
        for i = from, 1, -1 do
            local mark = marks[i]
            if label==nil or label==mark.label then
                return i, mark
            end
        end
    end
    return nil, nil
end

function Tokenizer:removeMark(index, pop)
    local marks = self._marks
    if marks==nil then
        return nil
    end
    local total = #marks
    if index<0 then
        index = total + 1 + index
    end
    if index<=0 or index>total then
        return nil
    end
    if pop then
        local removed = marks[index]
        for i = index, total do marks[i] = nil end
        return removed
    else
        return table.remove(marks, index)
    end
end

function Tokenizer:restore(label)
    local index, mark = self:lastMark(label)
    if index==nil then
        return false
    end
    self:removeMark(index, true)
    self._pos = mark.pos
    self._state = mark.state
    self._aheads.next = nil
    return true
end

function Tokenizer:_next(pos)
    if pos==nil or pos<0 or pos>self._len then
        return nil, pos
    end
    local source = self._source
    local rule, s, e, src, text = self._rules:matchRule(self._state, source, pos)
    if rule~=nil then
        local tok = {
            type = rule.type,
            start = s,
            ending = e+1,
            src = src,
            value = text or src,
            skip = rule.skip,
        }
        if not tok.skip and rule.callback~=nil then
            local tok2 = rule.callback(tok, self)
            if tok2==nil then
                tok.skip = true
            else
                tok = tok2
            end
        end
        return tok, tok.ending
    end

    -- eat one character
    local c = source:sub(pos, pos)
    local errtok = {type="error", start=pos, ending=pos+1, src=c, value=c, skip=true}
    local errorCallback = self._rules:getErrorCallback()
    if errorCallback~=nil then
        local tok2 = errorCallback(errtok, self)
        if tok2==nil then
            errtok.skip = true
        else
            errtok = tok2
        end
    end
    return errtok, errtok.ending
end

function Tokenizer:next(n)
    local tok, anode = self:_lookAhead(n)
    if anode~=nil then
        self._pos = anode.epos
        self._aheads.next = anode.next
    else
        self._pos = -1
        self._aheads.next = nil
    end
    return tok or (self._rules and self._rules:EOF())
end

function Tokenizer:_lookAhead(n)
    local tok
    local pos = self._pos
    local last = self._aheads
    local anode
    local idx = 1
    while idx<=(n or 1) do
        anode = last.next
        if anode then
            tok, pos = anode.tok, anode.epos
            last = anode
            idx = idx + 1
        else
            tok, pos = self:_next(pos)
            if tok==nil then break end
            if not tok.skip then
                anode = {
                    tok = tok, epos = pos,
                }
                last.next = anode
                last = anode
                idx = idx + 1
            end
        end
    end
    return tok, anode
end

function Tokenizer:lookAhead(n)
    return (self:_lookAhead(n)) or (self._rules and self._rules:EOF())
end

function Tokenizer:skipCharacters(len)
    local pos = self._pos + len
    if pos>self._len then
        self._pos = self._len + 1
    else
        self._pos = pos
    end
end

--------------------------------------------------------------------------------
-- Template token rules and parser
--------------------------------------------------------------------------------

-- token types
local kPlainText = 0
local kEvalBegin = "${"
local kInstBegin = "%{"

local kBlank = "BLANK"
local kId = "ID"
local kNumber = "NUMBER"
local kString = "STRING"
local kBoolean = "BOOLEAN"
local kNilValue = "NIL"

local EOF = {
    type = "EOF",
}

local escapeTable = {
    ["\b"] = "\\b",
    ["\f"] = "\\f",
    ["\n"] = "\\n",
    ["\r"] = "\\r",
    ["\t"] = "\\t",
    ["\""] = "\\\"",
    ["\\"] = "\\\\",
}
for k, v in pairs(escapeTable) do escapeTable[v] = k end

local function escapeString(s)
    return (string.gsub(s, "([\b\f\n\r\t\"\\])", function(c)
        return escapeTable[c] or c
    end))
end

local function unescapeString(s)
    return (string.gsub(s, "(\\.)", function(c)
        return escapeTable[c] or c
    end))
end

local templateKeywords = {
    ["not"] = {"ECHO", "INST"},
    ["and"] = {"ECHO", "INST"},
    ["or"] = {"ECHO", "INST"},

    ["set"] = {"INST"},
    ["if"] = {"INST"},
    ["else"] = {"INST"},
    ["for"] = {"INST"},
    ["in"] = {"INST"},
    ["end"] = {"INST"},
    ["break"] = {"INST"},
    ["continue"] = {"INST"},
    ["stop"] = {"INST"},
    ["return"] = {"INST"},

    ["silent"] = {"INST"},
    ["slurp"] = {"INST"},
    -- ["include"] = {"INST"},
}

local templateTokenRules = TokenRules:new {
    states = {
        {id="ECHO", exclusive=true,},
        {id="INST", exclusive=true,},
    },
    EOF = EOF,
    tokens = {
        {type=kEvalBegin, regex="%${",},
        {type=kInstBegin, regex="%%{",},
        {type=kPlainText, regex=".", callback = function (tok, tokenizer)
            local source = tokenizer:getSource()
            local s, e = source:find("[%%%$]{", tok.ending)
            if s~=nil then
                tok.ending = s
                tok.src = source:sub(tok.start, tok.ending-1)
            else
                tok.ending = source:len() + 1
                tok.src = source:sub(tok.start)
            end
            tok.value = tok.src
            tokenizer:skipCharacters(tok.ending - tokenizer:currentPosition())
            return tok
        end,},

        {type=kBlank, regex="%s+", states={"ECHO", "INST"}, skip=true,},

        {literal=".", states={"ECHO", "INST"},},
        {literal="(", states={"ECHO", "INST"},},
        {literal=")", states={"ECHO", "INST"},},
        {literal="[", states={"ECHO", "INST"},},
        {literal="]", states={"ECHO", "INST"},},
        {literal="{", states={"ECHO", "INST"},},
        {literal="}", states={"ECHO", "INST"},},
        {literal="<=", states={"ECHO", "INST"},},
        {literal=">=", states={"ECHO", "INST"},},
        {literal="<<", states={"ECHO", "INST"},},
        {literal=">>", states={"ECHO", "INST"},},
        {literal="==", states={"ECHO", "INST"},},
        {literal="!=", states={"ECHO", "INST"},},
        {literal="+", states={"ECHO", "INST"},},
        {literal="-", states={"ECHO", "INST"},},
        {literal="*", states={"ECHO", "INST"},},
        {literal="/", states={"ECHO", "INST"},},
        {literal=",", states={"ECHO", "INST"},},
        {literal="<", states={"ECHO"},},
        {literal=">", states={"ECHO"},},
        {literal="|", states={"ECHO"},},
        {literal="=", states={"INST"},},

        {type=kNumber, regex="[+-]?%d*%.%d+", states={"ECHO", "INST"}, callback = function (tok)
            tok.value = tonumber(tok.value); return tok
        end,},
        {type=kNumber, regex="[+-]?0[xX][%da-fA-F]+", states={"ECHO", "INST"}, callback = function (tok)
            tok.value = tonumber(tok.value, 16); return tok
        end,},
        {type=kNumber, regex="[+-]?%d+", states={"ECHO", "INST"}, callback = function (tok)
            tok.value = tonumber(tok.value); return tok
        end,},
        {type=kString, regex=[["([^"]*)"]], states={"ECHO", "INST"}, callback = function (tok)
            tok.value = unescapeString(tok.value); return tok
        end,},
        {type=kString, regex=[['([^']*)']], states={"ECHO", "INST"}, callback = function (tok)
            tok.value = unescapeString(tok.value); return tok
        end,},
        {type=kBoolean, literal="true", states={"ECHO", "INST"}, callback = function (tok)
            tok.value = true; return tok
        end,},
        {type=kBoolean, literal="false", states={"ECHO", "INST"}, callback = function (tok)
            tok.value = false; return tok
        end,},
        {literal="nil", states={"ECHO", "INST"},  callback = function (tok)
            tok.value = nil; return tok
        end,},
        {type=kId, regex="[%a_][%w_]*", states={"ECHO", "INST"}, callback = function (tok, tokenizer)
            local keyword = templateKeywords[tok.value]
            if keyword then
                local currentState = tokenizer:currentState()
                for _, state in ipairs(keyword) do
                    if state==currentState then
                        tok.type = tok.value
                        break
                    end
                end
            end
            return tok
        end},
    },

    errorCallback = function (errtok)
        print(string.format("Skip invalid character '%s' at %d.", errtok.src, errtok.start))
    end,
}


--- @class TemplateParser
local TemplateParser = {}

function TemplateParser:new(...)
    local obj = setmetatable({}, { __index = self, })
    obj:initialize(...)
    return obj
end

function TemplateParser:initialize(tokenizer)
    self._tokenizer = tokenizer
end

local function tokIsAnyOf(tok, ...)
    -- local arg = {n=select("#", ...), ...}
    local tokType = tok.type
    for i=1, arg.n do
        if tokType==arg[i] then return true end
    end
    return false
end

local function tokIsValue(tok)
    local tokType = tok.type
    return tokType==kNumber or tokType==kString or tokType==kBoolean or tokType==kNilValue
end

local function tokIsUnaryOp(tok)
    local tokType = tok.type
    return tokType=="not"--[[ or tokType=="-"]]
end

local binaryOperators = {
    --    priority, associativity
    ["<<"]     = {1, nil},
    ["<="]    = {1, nil},
    [">>"]     = {1, nil},
    [">="]    = {1, nil},
    ["=="]    = {1, nil},
    ["!="]    = {1, nil},

    ["+"]     = {2, "left"},
    ["-"]     = {2, "left"},
    ["or"]    = {2, "left"},

    ["*"]     = {3, "left"},
    ["/"]     = {3, "left"},
    ["and"]   = {3, "left"},
}
local function tokCheckBinaryOp(tok)
    local op = binaryOperators[tok.type]
    if op then
        return op[1], op[2]
    else
        return nil
    end
end


function TemplateParser:error(format, ...)
    local msg = string.format(format, ...)
    local tokenizer = self._tokenizer
    local pos = tokenizer:currentPosition()
    local start, ending = math.max(pos-10, 1), pos+10
    print("Around source:", pos, ":", tokenizer._source:sub(start, ending))
    print("Parse error:", debug.traceback(msg, 2))
    return msg
end

function TemplateParser:start()
    self._scopeStack = {}
    return self:block("main")
end

function TemplateParser:block(type)
    local tokenizer = self._tokenizer
    local block = {
        type = "block",
        range = {tokenizer:currentPosition(), -1},
    }
    local stackTop = #self._scopeStack + 1
    self._scopeStack[stackTop] = {
        type = type,
        block = block,
    }
    local errmsg
    tokenizer:changeState(nil) -- change to initial state
    while true do
        local tok = tokenizer:lookAhead()
        if tok==nil or tok==EOF then break end
        local node
        if tok.type==kPlainText then
            tokenizer:next()
            node = {
                type = "text",
                range = {tok.start, tok.ending},
                value = tok.value,
            }
        elseif tok.type==kEvalBegin then
            node, errmsg = self:evaluator()
        elseif tok.type==kInstBegin then
            node, errmsg = self:instruction()
            if node==nil and errmsg==nil then
                break
            end
        else
            return nil, self:error([[Got unexpected token `%s`]], tok.type)
        end
        if node==nil then
            return nil, errmsg
        end
        block[#block + 1] = node
        block.range[2] = node.range[2]
    end
    assert(#self._scopeStack == stackTop)
    self._scopeStack[stackTop] = nil
    return block
end

function TemplateParser:evaluator()
    local tokenizer = self._tokenizer
    local beginTok = tokenizer:next()
    tokenizer:changeState("ECHO")
    local expr, errmsg = self:expr()
    if expr==nil then
        return nil, errmsg
    end

    local evalNode = {
        type = "echo",
        expr,
    }

    local tok = tokenizer:next()

    local filter
    while tok.type=="|" do
        filter, errmsg = self:filter()
        if filter==nil then
            return nil, errmsg
        end
        evalNode[#evalNode + 1] = filter
        tok = tokenizer:next()
    end

    if tok.type~="}" then
        return nil, self:error([[`}` expected, got `%s`]], tok.type)
    end

    evalNode.range = {beginTok.start, tok.ending}
    tokenizer:changeState(nil)
    return evalNode
end

function TemplateParser:expr()
    return self:exprWithPriority(3)
end

function TemplateParser:exprWithPriority(priority)
    if priority==0 then
        return self:unary()
    end

    local tokenizer = self._tokenizer
    local exprNode, errmsg

    exprNode, errmsg = self:exprWithPriority(priority-1)
    if exprNode==nil then
        return nil, errmsg
    end

    while true do
        local tok = tokenizer:lookAhead()
        local opPriority, opAssociativity = tokCheckBinaryOp(tok)
        if opPriority==nil or opPriority<priority then
            break
        end
        tokenizer:next()
        local operandNode2, errmsg = self:exprWithPriority(priority-1)
        if operandNode2==nil then
            return nil, errmsg
        end
        exprNode = {
            type = "binary_expr",
            range = {exprNode.range[1], operandNode2.range[2]},
            op = tok.type,
            exprNode,
            operandNode2,
        }
        if opAssociativity==nil then
            break
        elseif opAssociativity=="right" then
            local leftNode = exprNode[1]
            if leftNode.type=="binary_expr" then
                exprNode[1] = leftNode[1]
                leftNode[1] = leftNode[2]
                leftNode[2] = exprNode[2]
                leftNode.range[1] = leftNode[1].range[1]
                leftNode.range[2] = leftNode[2].range[2]
            end
        end
    end

    return exprNode
end

function TemplateParser:unary()
    local tokenizer = self._tokenizer
    local tok = tokenizer:lookAhead()
    if tok.type=="not" or tok.type=="-" then
        tokenizer:next()
        local fnode, errmsg = self:factor()
        if fnode==nil then
            return nil, errmsg
        end
        local op = tok.type
        if op=="-" then op = "neg" end
        return {
            type = "unary_expr",
            range = {tok.start, fnode.range[2]},
            op = op,
            fnode,
        }
    end
    return self:factor()
end

function TemplateParser:factor()
    local tokenizer = self._tokenizer
    local tok = tokenizer:next()
    if tokIsValue(tok) then
        return self:exprConst(tok)
    elseif tok.type==kId then
        local node = {
            type = "ref_var",
            range = {tok.start, tok.ending},
            vname = tok.value,
        }
        while true do
            local postfix, errmsg
            tok = tokenizer:lookAhead()
            local tokType = tok.type
            local endTokType
            if tokType=="." then
                tokenizer:next()
                postfix, errmsg = self:exprGetField()
            elseif tokType=="[" then
                tokenizer:next()
                endTokType = "]"
                postfix, errmsg = self:exprGetDict()
            elseif tokType=="(" then
                tokenizer:next()
                endTokType = ")"
                postfix, errmsg = self:exprCallFunc()
            else
                break
            end
            if postfix==nil then
                return postfix, errmsg
            end
            if endTokType~=nil then
                tok = tokenizer:next()
                if tok.type~=endTokType then
                    return nil, self:error([[`%s` expected, got `%s`]], endTokType, tok.type)
                end
                postfix.range[2] = tok.ending
            end
            postfix[1] = node
            node = postfix
        end
        return node
    elseif tok.type=="(" then
        local subexpr, errmsg = self:expr()
        if subexpr==nil then
            return nil, errmsg
        end
        subexpr.range[1] = tok.start
        tok = tokenizer:next()
        if tok.type~=")" then
            return nil, self:error([[`)` expected, got `%s`]], tok.type)
        end
        subexpr.range[2] = tok.ending
        return subexpr
    else
        return nil, self:error([[Got unexpected token `%s`]], tok.type)
    end
end

function TemplateParser:filter()
    local tokenizer = self._tokenizer
    local tok = tokenizer:lookAhead()
    if tok.type~=kId then
        return nil, self:error([[`ID` expected, got `%s`]], tok.type)
    end
    local expr, errmsg = self:expr()
    if expr==nil then
        return nil, errmsg
    end
    local filter = {
        type = "filter",
        range = {tok.start, expr.range[2]},
        expr,
    }
    tok = tokenizer:lookAhead()
    if tok.type=="<" then
        tokenizer:next()
        filter, errmsg = self:exprListOrNone(filter, 2)
        if filter==nil then
            return nil, errmsg
        end
        tok = tokenizer:next()
        if tok.type~=">" then
            return nil, self:error([[`>` expected, got `%s`]], tok.type)
        end
        filter.range[2] = tok.ending
    end
    return filter
end

function TemplateParser:exprConst(tok)
    return {
        type = "const",
        range = {tok.start, tok.ending},
        value = tok.value,
        vtype = tok.type,
    }
end

function TemplateParser:exprGetField()
    local tokenizer = self._tokenizer
    local tok = tokenizer:next()
    if tok.type~=kId then
        return nil, self:error([[`ID` expected, got `%s`]], tok.type)
    end
    return {
        type = "get_field",
        range = {-1, tok.ending},
        key = tok.value,
        nil, -- place holder for expression prefix
    }
end

function TemplateParser:exprGetDict()
    local tokenizer = self._tokenizer
    local tok = tokenizer:lookAhead()
    if not (tokIsValue(tok) or tok.type==kId) then
        return nil, self:error([[`expr` expected, got `%s`]], tok.type)
    end
    local expr, errmsg = self:expr()
    if expr==nil then
        return nil, errmsg
    end
    return {
        type = "get_dict",
        range = {-1, -1},
        nil, -- place holder for expression prefix
        expr,
    }
end

function TemplateParser:exprCallFunc()
    local tokenizer = self._tokenizer
    return self:exprListOrNone({
        type = "call_func",
        range = {-1, -1},
        nil, -- place holder for expression prefix
    }, 2)
end

function TemplateParser:exprListOrNone(list, start)
    local tokenizer = self._tokenizer
    local tok = tokenizer:lookAhead()
    local n = 0
    while (tokIsValue(tok) or tok.type==kId) do
        local expr, errmsg = self:expr()
        if expr==nil then
            return nil, errmsg
        end
        list[(start or 1) + n], n = expr, n + 1
        tok = tokenizer:lookAhead()
        if tok.type~="," then break end
        tokenizer:next()
        tok = tokenizer:lookAhead()
    end
    return list
end

-- {literal="set", states={"INST"},},
-- {literal="if", states={"INST"},},
-- {literal="else", states={"INST"},},
-- {literal="for", states={"INST"},},
-- {literal="in", states={"INST"},},
-- {literal="end", states={"INST"},},
-- {literal="break", states={"INST"},},
-- {literal="continue", states={"INST"},},
-- {literal="stop", states={"INST"},},
-- {literal="return", states={"INST"},},

-- {literal="silent", states={"INST"},},
-- {literal="slurp", states={"INST"},},

function TemplateParser:instruction()
    local tokenizer = self._tokenizer
    local beginTok = tokenizer:next()
    self._instBeginTok = beginTok
    tokenizer:changeState("INST")

    local node, errmsg
    local tok = tokenizer:lookAhead()
    local instType = tok.type
    if instType=="set" then
        node, errmsg = self:instSetValue()
    elseif instType=="if" then
        node, errmsg = self:branches()
    elseif instType=="else" then
        return nil, nil
    elseif instType=="for" then
        node, errmsg = self:forLoop()
    elseif instType=="end" then
        return self:instEndScope()
    else
        return nil, self:error([[Got unexpected token `%s`]], tok.type)
    end

    if node==nil then
        return nil, errmsg
    end

    tok = tokenizer:next()
    if tok.type~="}" then
        return nil, self:error([[`}` expected, got `%s`]], tok.type)
    end

    node.range = {beginTok.start, tok.ending}
    local child = node[1]
    while child~=nil do
        if child.range[1]>0 then
            break
        end
        child.range[1] = beginTok.start
        child = child[1]
    end
    tokenizer:changeState(nil)
    self._instBeginTok = nil

    return node
end

function TemplateParser:instSetValue()
    local tokenizer = self._tokenizer
    local tok = tokenizer:next()
    -- assert(tok.type=="set")
    local node = {
        type = "set_value",
        -- range = {-1, -1},
    }
    tok = tokenizer:next()
    if tok.type~=kId then
        return nil, self:error([[`ID` expected, got `%s`]], tok.type)
    end
    node[1] = {
        type = "ref_var",
        range = {tok.start, tok.ending},
        vname = tok.value,
    }
    tok = tokenizer:next()
    if tok.type~="=" then
        return nil, self:error([[`=` expected, got `%s`]], tok.type)
    end
    local expr, errmsg = self:expr()
    if expr==nil then
        return expr, errmsg
    end
    node[2] = expr
    return node
end

function TemplateParser:branches()
    local tokenizer = self._tokenizer
    -- local tok = tokenizer:next()
    -- assert(tok.type=="if")
    local branchesNode = {
        type = "branches",
        range = {-1, -1},
    }
    local branchCount = 0
    while true do
        local condNode, blockNode, errmsg
        local branchNode = {
            type = nil,
            range = {self._instBeginTok.start, -1},
        }
        local tok = tokenizer:lookAhead()
        if tok.type=="if" then
            tokenizer:next()
            condNode, errmsg = self:expr()
            if condNode==nil then
                return nil, errmsg
            end
            branchNode.type = branchCount==0 and "if" or "else_if"
            branchNode[1] = condNode
        else
            branchNode.type = "else"
        end
        tok = tokenizer:next()
        if tok.type~="}" then
            return nil, self:error([[`}` expected, got `%s`]], tok.type)
        end
        tokenizer:changeState(nil)

        local block
        block, errmsg = self:block("if")
        if block==nil then
            return nil, errmsg
        end
        branchNode[#branchNode + 1] = block
        branchNode.range[2] = block.range[2]
        branchCount = branchCount + 1
        branchesNode[branchCount] = branchNode

        tok = tokenizer:lookAhead()
        if tok.type~="else" then
            break
        end
        if condNode==nil then
            return nil, self:error([[Got unexpected token `%s`]], tok.type)
        end
        tokenizer:next()
    end
    return branchesNode
end

function TemplateParser:forLoop()
    local tokenizer = self._tokenizer
    local tok = tokenizer:next()
    -- assert(tok.type=="for")
    local node = {
        type = "for",
        range = {-1, -1},
    }
    tok = tokenizer:next()
    if tok.type~=kId then
        return nil, self:error([[`ID` expected, got `%s`]], tok.type)
    end
    node.vname = tok.value
    tok = tokenizer:next()
    if tok.type~="=" then
        return nil, self:error([[`=` expected, got `%s`]], tok.type)
    end
    local initExpr, errmsg = self:expr()
    if initExpr==nil then
        return nil, errmsg
    end
    tok = tokenizer:next()
    if tok.type~="," then
        return nil, self:error([[`,` expected, got `%s`]], tok.type)
    end
    local finalExpr, errmsg = self:expr()
    if finalExpr==nil then
        return nil, errmsg
    end
    node[1] = initExpr
    node[2] = finalExpr
    tok = tokenizer:lookAhead()
    if tok.type=="," then
        tokenizer:next()
        local stepExpr, errmsg = self:expr()
        if stepExpr==nil then
            return nil, errmsg
        end
        node[3] = stepExpr
        node.range[2] = stepExpr.range[2]
    else
        node.range[2] = finalExpr.range[2]
    end
    tok = tokenizer:next()
    if tok.type~="}" then
        return nil, self:error([[`}` expected, got `%s`]], tok.type)
    end
    tokenizer:changeState(nil)

    local block, errmsg = self:block("for")
    if block==nil then
        return nil, errmsg
    end
    return {
        type = "for_loop",
        range = {-1, -1},
        node,
        block,
    }
end

function TemplateParser:instEndScope()
    local tokenizer = self._tokenizer
    local tok = tokenizer:next()
    local scope = self._scopeStack[#self._scopeStack]
    if scope==nil or scope.type=="main" then
        return nil, self:error([[Got unexpected token `%s`]], tok.type)
    end
    local tok = tokenizer:lookAhead()
    if tok.type~="}" then
        tok = tokenizer:next()
        if tok.type~=scope.type then
            return nil, self:error([[Got unexpected token `%s`]], tok.type)
        end
    end
    return nil, nil
end

function TemplateParser:dumpAST(root, name)
    local function printf(fmt, ...)
        print(string.format(fmt, ...))
    end
    if root==nil then
        printf("%s : nil", name or "root")
        return
    end
    local fixedKeys = {["type"] = true, ["range"] = true}
    local function _dumpFields(node, indents)
        if node==nil then return end
        printf("%stype : %s", indents, tostring(node.type))
        printf("%srange : [%d, %d)", indents, node.range[1], node.range[2])
        for k, v in pairs(node) do
            if not fixedKeys[k] and type(k)=="string" then
                printf("%s%s : `%s`", indents, k, escapeString(tostring(v)))
            end
        end
        for i, v in ipairs(node) do
            -- printf("%s- [%d] {", indents, i)
            printf("%s- {", indents, i)
            _dumpFields(v, indents.."  ")
            printf("%s}", indents)
        end
    end
    printf("%s = {", name or "root")
    _dumpFields(root, "  ")
    printf("}")
end


--------------------------------------------------------------------------------
--- @class Template
--------------------------------------------------------------------------------
local Template = {}

local DummyEnv = {}

function Template:new(...)
    local obj = setmetatable({}, { __index = self, })
    obj:initialize(...)
    return obj
end

function Template:initialize(format)
    self._format = format
    self._ast = nil
    self._genFunc = nil
end

function Template:compile(options)
    local ast = self._ast
    if ast==nil then
        local errmsg
        ast, errmsg = self:parse(self._format)
        if ast==nil then
            return false, errmsg
        end
    end
    self:prepare(ast, self._format, options)
    return true
end

function Template:parse(source, options)
    local tokenizer = Tokenizer:new(templateTokenRules)
    local parser = TemplateParser:new(tokenizer)
    tokenizer:setSource(source)
    return parser:start()
end

function Template:dumpAST(name)
    return TemplateParser:dumpAST(self._ast, name)
end

function Template:prepare(ast, source, options)
    self._ast = ast
    self._genFunc = self:_buildGenerator(ast, source, options)
    self._specFunc = self:_buildSpecializer(ast, source, options)
end

--------------------------------------------------------------------------------
-- Template evaluators
--------------------------------------------------------------------------------

local operatorFunctions = {
    ["<<"]     = function (a, b) return a < b end,
    ["<="]    = function (a, b) return a <= b end,
    [">>"]     = function (a, b) return a > b end,
    [">="]    = function (a, b) return a >= b end,
    ["=="]    = function (a, b) return a == b end,
    ["!="]    = function (a, b) return a ~= b end,

    ["+"]     = function (a, b) return a + b end,
    ["-"]     = function (a, b) return a - b end,
    ["or"]    = function (a, b) return a or b end,

    ["*"]     = function (a, b) return a * b end,
    ["/"]     = function (a, b) return a / b end,
    ["and"]   = function (a, b) return a and b end,

    ["not"]   = function (a) return not a end,
    -- ["neg"]   = function (a) return -a end,
    -- ["pos"]   = function (a) return a end,
}

local _eval, _applyFilter

function _eval(node, env)
    local nodetype = node.type
    if nodetype=="ref_var" then
        return env[node.vname]
    elseif nodetype=="const" then
        return node.value
    else
        local prevalue = _eval(node[1], env)
        if prevalue==nil then return nil end
        if nodetype=="get_field" then
            return prevalue[node.key]
        elseif nodetype=="get_dict" then
            local key = _eval(node[2], env)
            return prevalue[key]
        elseif nodetype=="call_func" then
            if node[2]==nil then
                return prevalue()
            end
            local args, n = {}, #node
            for i=2, #node do
                local val = _eval(node[i], env)
                args[i-1] = val
            end
            return prevalue(unpack(args, 1, n-1))
        elseif nodetype=="binary_expr" then
            local opfunc = operatorFunctions[node.op]
            if opfunc~=nil then
                local operand1 = _eval(node[1], env)
                if operand1==nil then return nil end
                local operand2 = _eval(node[2], env)
                if operand2==nil then return nil end
                return opfunc(operand1, operand2)
            end
        elseif nodetype=="unary_expr" then
            local opfunc = operatorFunctions[node.op]
            if opfunc~=nil then
                local operand1 = _eval(node[1], env)
                if operand1==nil then return nil end
                return opfunc(operand1)
            end
        end
    end
    return nil
end

function _applyFilter(node, value, env, filters)
    local func = _eval(node[1], filters)
    if func==nil then
        return value
    end
    if node[2]==nil then
        return func(value)
    end
    local args, n = {}, #node
    for i=2, #node do
        local val = _eval(node[i], env)
        args[i-1] = val
    end
    return func(value, unpack(args, 1, n-1))
end


function Template:_buildGenerator(ast, source, options)
    local _generate, _echo, _setValue, _branches, _forLoop

    local delayEval = options and options.enableDelayEval
    local applyFiltersOnNilValue = options and options.applyFiltersOnNilValue
    function _echo(node, env, filters)
        local value = _eval(node[1], env)
        for i=2, #node do
            if value==nil and not applyFiltersOnNilValue then break end
            value = _applyFilter(node[i], value, env, filters)
        end
        if value==nil then
            if delayEval then
                return source:sub(node.range[1], node.range[2]-1)
            end
            return ""
        end
        return tostring(value)
    end

    local localEnvSign = {}
    local function localEnv(env)
        if rawget(env, localEnvSign)==localEnvSign then return env end
        return setmetatable({[localEnvSign]=localEnvSign}, {__index = env})
    end

    function _setValue(node, env)
        env = localEnv(env)
        env[node[1].vname] = _eval(node[2], env)
        return env
    end

    function _branches(node, slices, env, filters)
        for i, branch in ipairs(node) do
            if branch.type=="else" then
                _generate(branch[1], slices, env, filters)
                break
            elseif _eval(branch[1], env) then
                _generate(branch[2], slices, env, filters)
                break
            end
        end
    end

    function _forLoop(node, slices, env, filters)
        local forNode = node[1]
        local blockNode = node[2]
        local varName = forNode.vname
        local initValue = _eval(forNode[1], env)
        if initValue==nil then return end
        local finalValue = _eval(forNode[2], env)
        if finalValue==nil then return end
        local stepValue = 1
        if forNode[3] then
            stepValue = _eval(forNode[3], env)
            if stepValue==nil then return end
        end
        for i=initValue, finalValue, stepValue do
            local env2 = localEnv(env)
            env2[varName] = i
            _generate(blockNode, slices, env2, filters)
        end
    end

    function _generate(node, slices, env, filters)
        for i, subnode in ipairs(node) do
            local subtype = subnode.type
            if subtype=="text" then
                slices[#slices + 1] = subnode.value
            elseif subtype=="echo" then
                slices[#slices + 1] = _echo(subnode, env, filters)
            elseif subtype=="set_value" then
                env = _setValue(subnode, env)
            elseif subtype=="branches" then
                _branches(subnode, slices, env, filters)
            elseif subtype=="for_loop" then
                _forLoop(subnode, slices, env, filters)
            end
        end
    end

    return function (env, filters)
        local slices = {}
        _generate(ast, slices, env, filters)
        return table.concat(slices, "")
    end
end

function Template:_buildSpecializer(ast, source, options)
    local _specialize, _simplify, _echo, _branches, _forLoop

    local applyFiltersOnNilValue = options and options.applyFiltersOnNilValue
    function _echo(node, env, filters)
        local value = _eval(node[1], env)
        for i=2, #node do
            if value==nil and not applyFiltersOnNilValue then break end
            value = _applyFilter(node[i], value, env, filters)
        end
        return value~=nil and tostring(value) or node
    end

    function _branches(node, env, filters)
        local result
        for i, branch in ipairs(node) do
            if branch.type=="else" then
                return _simplify(_specialize(branch[1], env, filters))
            else
                local cond = _eval(branch[1], env)
                if cond==nil then return node end
                if cond then
                    return _simplify(_specialize(branch[2], env, filters))
                end
            end
        end
        return node
    end

    function _forLoop(node, env, filters)
        local forNode = node[1]
        local blockNode = node[2]
        local varName = forNode.vname
        local initValue = _eval(forNode[1], env)
        if initValue==nil then return node end
        local finalValue = _eval(forNode[2], env)
        if finalValue==nil then return node end
        local stepValue = nil
        if forNode[3] then
            stepValue = _eval(stepValue, env)
            if stepValue==nil then return node end
        end
        return {
            type = "for_loop",
            range = node.range,
            {
                type = "for",
                range = forNode.range,
                vname = varName,
                {type="const", range={-1,-1}, value=initValue,},
                {type="const", range={-1,-1}, value=finalValue,},
                stepValue~=nil and {type="const", range={-1,-1}, value=stepValue,} or nil,
            },
            -- blockNode,
            _specialize(blockNode, env, filters)
        }
    end

    function _simplify(result)
        if result.type~="block" then
            return result
        end
        local n = #result
        if n==0 then
            return nil
        elseif n==1 and result[1].type=="text" then
            return result[1].value
        else
            return result
        end
    end

    function _specialize(node, env, filters)
        assert(node.type=="block")
        local slices = {}
        local newNode = {
            type = "block",
            range = node.range,
        }
        local i = 0
        while true do
            i = i + 1
            local subnode = node[i]
            local subtype = subnode and subnode.type
            local result
            if subtype=="text" then
                result = subnode.value
            elseif subtype=="echo" then
                result = _echo(subnode, env, filters)
            elseif subtype=="branches" then
                result = _branches(subnode, env, filters)
            elseif subtype=="for_loop" then
                result = _forLoop(subnode, env, filters)
            else
                result = subnode
            end
            if type(result)=="string" then
                slices[#slices + 1] = result
            else
                local n = #slices
                if n==1 and node[i-1].type=="text" then
                    newNode[#newNode + 1] = node[i-1]
                    slices[1] = nil
                elseif n>=1 then
                    newNode[#newNode + 1] = {
                        type = "text",
                        range = {-1,-1},
                        value = table.concat(slices, ""),
                    }
                    slices = {}
                end
                if subnode==nil then
                    break
                end
                if result~=nil then
                    if result.type=="block" then
                        for _, subresult in ipairs(result) do
                            newNode[#newNode + 1] = subresult
                        end
                    else
                        newNode[#newNode + 1] = result
                    end
                end
            end
        end
        return newNode
    end

    return function (env, filters)
        -- return _specialize(ast, setmetatable({}, {__index = env}), filters)
        return _specialize(ast, env, filters)
    end
end

--- @desc Generate string content with given environment.
--- @param env table @ the environment
--- @param filters table @ [Optional] filters mapping (a table maps names to functions)
--- @return a string
function Template:render(env, filters)
    local genFunc = self._genFunc
    if genFunc==nil then
        self:compile()
        genFunc = self._genFunc
    end
    if genFunc==nil then
        return nil
    end
    if env == nil then
        env = DummyEnv
    end
    return genFunc(env, setmetatable({}, {__index = function(t, k)
        local v = (filters and filters[k]) or env[k]
        if v~=nil then rawset(t, k, v) end
        return v
    end}))
end

--- @desc Create a new template by replacing the variables in the environment
---     and leaving the others unchanged.
--- @param env table @ the environment
--- @param filters table @ [Optional] filters mapping (a table maps names to functions)
--- @param options table @ options for preparing the result template
--- @return a new template object
function Template:specialize(env, filters, options)
    local specFunc = self._specFunc
    if specFunc==nil then
        self:compile()
        specFunc = self._specFunc
    end
    if specFunc==nil then
        return nil
    end
    if env == nil then
        env = DummyEnv
    end
    local ast = specFunc(env, setmetatable({}, {__index = function(t, k)
        local v = (filters and filters[k]) or env[k]
        if v~=nil then rawset(t, k, v) end
        return v
    end}))
    if ast==nil then
        return nil
    end
    local specialized = Template:new(self._format)
    specialized:prepare(ast, self._format, options)
    return specialized
end

---------- Filter management methods ----------

local globalFilters = {}
--- @desc Map filter name to a function. It will affect all text templates.
--- @param name string @ the filter name
--- @param filterFunc function @ the filter function: string (string[, ...])
function Template.mapGlobalFilter(name, filterFunc)
    if name==nil then
        return
    end
    globalFilters[name] = filterFunc
end

--- @desc Map filter name to a function. It affects only this template object.
--- @param name string @ the filter name
--- @param filterFunc function @ the filter function: string (string[, ...])
function Template:mapFilter(name, filterFunc)
    if name==nil then
        return
    end
    if self._filters==nil then
        self._filters = {}
    end
    self._filters[name] = filterFunc
end

--- @desc Create a filter registry combined with builtin/global filters.
--- @param userFilters table @ [Optional] user defined filters
function Template:combinedFilters(userFilters)
    local builtinFilters = self._filters
    return setmetatable({}, {__index = function(t, k)
        local v = (userFilters and userFilters[k])
                    or (builtinFilters and builtinFilters[k])
                    or globalFilters[k]
        if v~=nil then rawset(t, k, v) end
        return v
    end})
end

return Template
