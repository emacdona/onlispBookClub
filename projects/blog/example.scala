#!/usr/bin/env scala

// Slope-intercept form of line
def y(m:Float, x:Float, b:Float) = m * x + b

def slopeInterceptLine(slope: Float, intercept: Float) = y(slope, _, intercept)

def y1 = slopeInterceptLine(2,0)
def y2 = slopeInterceptLine(2,-1)

val indexes :List[Float] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

println(List(indexes.map(y1), "\n", indexes.map(y2)))

