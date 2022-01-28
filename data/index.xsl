<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output
        doctype-system="about:legacy-compat"
        encoding="UTF-8"
        media-type="text/html"
        method="html" />

    <xsl:template match="/root">
        <html>
            <head>
                <title>Haskell Weekly</title>
                <link rel="stylesheet" href="/static/style" />
            </head>
            <body>
                <h1>Haskell Weekly</h1>
            </body>
        </html>
    </xsl:template>

    <xsl:template match="/status">
        <html>
            <head>
                <title>Haskell Weekly</title>
                <link rel="stylesheet" href="/static/style" />
            </head>
            <body>
                <h1>Haskell Weekly</h1>
                <p>
                    <xsl:value-of select="normalize-space(code)" />:
                    <xsl:value-of select="message" />
                </p>
            </body>
        </html>
    </xsl:template>
</xsl:stylesheet>
