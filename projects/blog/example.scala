#!/usr/bin/env scala

// Slope-intercept form of line
def y(m:Float, x:Float, b:Float) = m * x + b

def slopeInterceptLine(slope: Float, intercept: Float) = y(slope, _, intercept)

def y1 = slopeInterceptLine(2,0)
def y2 = slopeInterceptLine(2,-1)

println(y1(1))
println(y1(2))
println(y1(3))
println(y1(4))
println()
println(y2(1))
println(y2(2))
println(y2(3))
println(y2(4))
