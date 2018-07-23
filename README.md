# MoeDB
  
A database plugin for Nukkit and Nukkit developers   
This plugin have no commands and tasks, just provide databases for other developers.  
Other plugins can easily use databases.

## Dependency
If you want to depend this plugin:
### Maven Repository

1. Add `repository` in `repositories`
    ```xml
    <repositories>
        <repository>
            <id>him188-moedb</id>
            <url>http://repo.him188.moe:8081/repository/moedb</url>
        </repository>
    ```
2. Add `dependency` in `build.dependencies`
    ```xml
    <dependency>
        <groupId>net.mamoe</groupId>
        <artifactId>moedb</artifactId>
        <version>1.0-SNAPSHOT</version>
    </dependency>
    ```