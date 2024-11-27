plugins {
    id("java")
    id("org.jetbrains.intellij.platform") version "2.0.1"
}

group = "io.github.donkirkby"
version = "4.12.0"

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
        intellijIdeaCommunity("2024.3")

        // "Pythonid" for ultimate, "PythonCore" for community. Find compatible
        // ultimate versions at https://plugins.jetbrains.com/plugin/631
        // community versions at https://plugins.jetbrains.com/plugin/7322
        plugin("PythonCore:243.21565.211")

        instrumentationTools()
        pluginVerifier()
    }
}

sourceSets {
    main {
        resources {
            srcDirs("../plugin/PySrc")
            exclude("**/*.pyc")
            exclude("*.egg-info")
        }
    }
}

// Configure IntelliJ Platform Gradle Plugin - read more: https://plugins.jetbrains.com/docs/intellij/tools-intellij-platform-gradle-plugin-extension.html
intellijPlatform {
    pluginVerification {
        ides {
            // recommended()
            // Available versions listed at https://www.jetbrains.com/idea/download/other.html
            ides(listOf("IC-2022.2", "IC-2023.1", "IC-2024.3"))
        }
    }
}

tasks {
    patchPluginXml {
        sinceBuild = "222.3345.118"
        untilBuild = provider { null }
    }
}