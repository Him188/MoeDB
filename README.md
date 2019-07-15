# MoeDB
  
A database plugin for Nukkit and Nukkit developers   
This plugin have no commands and tasks, just provide databases for other developers.  
Other plugins can easily use databases.


## Download [![Build Status](https://travis-ci.org/Him188/MoeDB.svg?branch=master)](https://travis-ci.org/Him188/MoeDB)
- [TeamCity](http://mamoe.net:2333/job/MoeDB)

## Dependency
If you want to depend this plugin:

### Maven Repository

1. Add `repository` in `repositories`
    ```xml
    <repositories>
        <repository>
            <id>mamoe-repo</id>
            <url>http://mamoe.net:8081/repository/public/</url>
        </repository>
    ```
2. Add `dependency` in `build.dependencies`
    ```xml
    <dependency>
        <groupId>net.mamoe</groupId>
        <artifactId>moedb</artifactId>
        <version>LATEST</version>
    </dependency>
    ```
