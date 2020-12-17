# AoC 2017
Using C++ 17

## VSCode
Settings for running and debugging.
### launch.json
```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "clang++ - Build and debug active file",
            "type": "cppdbg",
            "request": "launch",
            "program": "${workspaceFolder}/${relativeFileDirname}/bin/${fileBasenameNoExtension}",
            "args": [],
            "stopAtEntry": false,
            "cwd": "${workspaceFolder}/${relativeFileDirname}",
            "environment": [],
            "externalConsole": false,
            "MIMode": "lldb",
            "preLaunchTask": "C/C++: clang++ build active file"
        }
    ]
}
```

### tasks.json
```json
{
    "tasks": [
        {
            "type": "shell",
            "label": "createbin",
            "options": {
                "cwd": "${workspaceFolder}"
            },
            "command": "mkdir",
            "args": [
                "-p",
                "${fileDirname}/bin"
            ],
            "presentation": {
                "echo": false,
                "reveal": "silent",
                "focus": false,
                "panel": "dedicated",
                "showReuseMessage": false,
                "clear": false
            }
        },
        {
            "type": "cppbuild",
            "label": "C/C++: clang++ build active file",
            "command": "/usr/bin/clang++",
            "args": [
                "-std=c++17",
                "-stdlib=libc++",
                "-g",
                "${file}",
                "-o",
                "${fileDirname}/bin/${fileBasenameNoExtension}"
            ],
            "options": {
                "cwd": "${workspaceFolder}"
            },
            "problemMatcher": [
                "$gcc"
            ],
            "group": {
                "kind": "build",
                "isDefault": true
            },
            "presentation": {
                "echo": true,
                "reveal": "silent",
                "focus": false,
                "panel": "shared",
                "showReuseMessage": true,
                "clear": false
            },
            "dependsOn": "createbin",
            "detail": "Task generated by Debugger."
        }
    ],
    "version": "2.0.0"
}
```