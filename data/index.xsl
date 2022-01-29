<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output
        doctype-system="about:legacy-compat"
        encoding="UTF-8"
        media-type="text/html"
        method="html" />

    <xsl:template match="/root">
        <html lang="en-US">
            <head>
                <meta charset="UTF-8" />
                <meta name="viewport" content="initial-scale = 1, width = device-width" />
                <title>Haskell Weekly</title>
                <link rel="stylesheet" href="/static/style" />
            </head>
            <body>
                <header class="navbar navbar-dark bg-dark">
                    <div class="container">
                        <a class="navbar-brand" href="/">
                            Haskell Weekly
                        </a>
                    </div>
                </header>
                <main class="container my-3">
                    TODO
                </main>
                <footer class="border-top container my-3 pt-3 text-muted">
                    <a href="https://github.com/haskellweekly/hw">
                        github.com/haskellweekly/hw
                    </a>
                </footer>
            </body>
        </html>
    </xsl:template>
</xsl:stylesheet>
