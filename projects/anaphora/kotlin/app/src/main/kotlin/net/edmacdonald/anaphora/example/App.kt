package net.edmacdonald.anaphora.example

fun main() {
    val l = listOf(1, 2, 3)

    println("Anaphor: ${
        l.map { it * 100 }
    }")

    println("Lambda: ${
        l.map { element -> element * 100 }
    }")
}
