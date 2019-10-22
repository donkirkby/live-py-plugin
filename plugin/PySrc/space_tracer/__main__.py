from .main import IS_PYODIDE, main, web_main

if IS_PYODIDE:
    web_main()
else:
    main()
