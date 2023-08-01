-- Test different types of number literals in Lua

-- Integers
3 -- should succeed
345 -- should succeed
0xff -- should succeed
0xBEBADA -- should succeed

-- Floats
3.0 -- should succeed
3.1416 -- should succeed
314.16e-2 -- should succeed
0.31416E1 -- should succeed
34e1 -- should succeed

-- Floats that should work but don't (not yet implemented)
-- 0x0.1E -- will fail
-- 0xA23p-4 -- will fail
-- 0X1.921FB54442D18P+1 -- will fail
