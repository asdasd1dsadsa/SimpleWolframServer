# Simple Wolfram Language Server

This server is written by almost pure Wolfram Language for heuristic purposes.

This server is inspired by [wolfram-server](https://github.com/arnoudbuzing/wolfram-server) but attempts to give a more complicated application of built-in functions for web development.

I may update this code in the future. And you can feel free to submit PRs if you created some applications which meet common requirements.


## Usage of files and functions
### StartServer.wls
Start a ```SocketListener``` with user-defined configuration. Print some useful message.

### WolframServer.wl
* ```HTTPRespond```: Process socket data, print a log string and then close client ```SocketObject```. The default response is decided by ```WolframServerService`$MainURLDispatcher```.

### WolframServerService.wl
* ```$MainURLDispatcher```: The default URLs dispatcher which dispatch URL to ```$IndexService```, ```$ServiceDispatcher``` (Web Service), ```$APIDispatcher``` (JSON Response), a ```GetResource``` function which meets requirements for files and a dispatcher in ```WolframServerUserService.wl```.

### WolframServerTool.wl
* ```ToMIME```: Convert file format postfixes to MIME type strings. It works for ```WolframServerService`GetResource``` to provide ```HTTPResponse[<|"ContentType" -> "MIMEType"|>]```. Not finished yet.
* ```ResourceLocalize```: Replace the resources link to wolframcloud.com by local link. It works for ```WolframServerService`GetResource```.
* ```ApacheLog```: Give an Apache-style log. Not a complete function yet.
* ```MakeFormRules```: Convert body bytes in the ```HTTPRequest``` received to an association. It works for ```FormFunction``` which requires ```HTTPRequestData["FormRules"]```.

### WolframServerManageTool.wl
* ```ShutdownServer```
* ```StartServer```
* ```ZMQTaskWait```: Hold the main loop to forbid process to exit. It works for StartServer.wls in the command line environment.
* ```ShowLog```: Import the log file in the server directory as a string. The default log file is "nohup.out".

### WolframServerUserService.wl
There is a simple note service implemented, which is a string list that can be operated by the web UI provided by some ```FormFunction```s.

### TestClient.wls
Access the root path of local host on port 8080.

### CopyResource.wls
Copy cloud resources to be localized from Wolfram Engine's installation directory. It works for ```WolframServerTool`ResourceLocalize```.

## Note
* To provide services to users as a server, you need a proper license.
* There's no security features implemented in ```WolframServerService`EvaluationService```. Users can evaluate anything. Possible expressions includes ```RunProcess```, ```Quit``` etc..