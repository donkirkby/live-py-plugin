plugins {
    id("java")
    id("org.jetbrains.intellij.platform") version "2.0.1"
}

group = "io.github.donkirkby"
version = "4.11.4"

repositories {
    mavenCentral()

    intellijPlatform {
        defaultRepositories()
    }
}

// Configure Gradle IntelliJ Plugin
// Read more: https://plugins.jetbrains.com/docs/intellij/tools-intellij-platform-gradle-plugin.html
dependencies {
    intellijPlatform {
        intellijIdeaCommunity("2024.2.2")

        plugin("PythonCore:243.15521.24")

        pluginVerifier()
    }
}
