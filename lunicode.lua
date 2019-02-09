#!/usr/bin/lua

--[[ test.lua

Probing utf8 input/output.
--]]

local TEST = nil

--~ local TEST = { ['decode'] = true,
               --~ ['encode'] = true, }

local bit32 = require 'bit32'

local lib = {}

local MASK_HIGH = { 128, 192, 224, 240, 248 }
local MASK_LOW  = { 1,   3,   7,   15,  31,  63 }
local HEAD = { 192, 224, 240 }
local UPPER_LIMITS = { 0x800, 0x10000, 0x110000 }

--[[ Organization of HEADS is
          mh = {d, mc, b, n}
    where
        mh: decimal number of bitmask for head
        d:  decimal number of multibyte head
        mc  decimal number of bitmask for rest of byte
        n:  total number of bytes in codepoint
--]]
local HEADS = {
    [MASK_HIGH[3]] = { HEAD[1], MASK_LOW[5], 2 },
    [MASK_HIGH[4]] = { HEAD[2], MASK_LOW[4], 3 },
    [MASK_HIGH[5]] = { HEAD[3], MASK_LOW[3], 4 }
}
local HEAD_CONT = 128
local REPLACEMENT = 0xfffd

local function munch_at_n(s, n, maxn)
    local ptr = n
    local c = string.byte(s:sub(n,n))
    
    if bit32.band(c, MASK_HIGH[1]) == 0 then
        return c, 1, nil
    end
    
    for m, a in pairs(HEADS) do
        if bit32.band(c, m) == a[1] then
            local nextra = a[3] - 1
            if n + nextra > maxn then
                return nil, nil, 'string ends mid codepoint'
            end
            local val = bit32.band(c, a[2])
            for i = n+1,n+nextra do
                c = string.byte(s:sub(i,i))
                if bit32.band(MASK_HIGH[2], c) ~= HEAD_CONT then
                    return REPLACEMENT, nextra+1, nil
                end
                val = bit32.lshift(val, 6) + bit32.band(c, MASK_LOW[6])
            end
            return val, nextra+1, nil
        end
    end
    
    return nil, nil, 'invalid unicode string'
end

function lib.decode(s)
    ptr = 1
    N = #s
    points = {}
    
    while ptr <= N do
        p, adv, err = munch_at_n(s, ptr, N)
        if p then
            table.insert(points, p)
            ptr = ptr + adv
        else
            return points, err
        end
    end
    
    return points, nil
end

function lib.encode(a)
    local chunks = {}
    for _, c in ipairs(a) do
        if c < 0x80 then
            table.insert(chunks, string.char(c))
        else
            local t = {}
            local err = true
            for n, lim in ipairs(UPPER_LIMITS) do
                if c < lim then
                    for _ = 1,n do
                        local x = bit32.bor(HEAD_CONT,
                                    bit32.band(MASK_LOW[6], c))
                        table.insert(t, x)
                        c = bit32.rshift(c, 6)
                    end
                    local x = bit32.band(MASK_LOW[6-n], c)
                    x = bit32.bor(HEAD[n], x)
                    table.insert(chunks, string.char(x))
                    for i = n,1,-1 do
                        table.insert(chunks, string.char(t[i]))
                    end
                    err = nil
                    break
                end
            end
        end
    end
    
    return table.concat(chunks, '')
end

if TEST then

    local parr = { 65, 66, 67, 68, 69, 32, 97, 98, 99, 100 }

    if TEST.decode then
        print('feed me a unicode string to test decoding')
        local s = io.stdin:read('*line')
        parr, err = lib.decode(s)
        if parr then
            print(table.concat(parr, ' '))
        else
            print('ERROR: ' .. err)
        end
    end
    
    if TEST.encode then
        local s = lib.encode(parr)
        print(s)
    end

else
    return lib
end