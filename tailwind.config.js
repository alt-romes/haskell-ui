module.exports = {
    content: ["./src/**/*.hs", "./app/**/*.hs", "./*.hs"],
    theme: {
        extend: {
            fontSize: {
                '2xs': ['0.7rem', '1rem']
            }
        },
    },
    plugins: [],
    safelist: [
        {
            pattern: /h-(1|2|3|4|5|6|7|8|9|10|11|12|14|16|20|24)/,
        },
        {
            pattern: /w-(1|2|3|4|5|6|7|8|9|10|11|12|14|16|20|24)/,
        },
        {
            pattern: /grid-cols-(1|2|3|4|5|6|7|8|9|10|11|12)/,
        },
        {
            pattern: /gap-(1|1.5|2|2.5|3|3.5|4|5|6|7||8|9|10|11|12|14|16|20|24)/,
        },
        {
            pattern: /text-(.+?)-(.+)/,
        },
        {
            pattern: /border-(.+?)-(.+)/,
            variants: ["focus"]
        },
        {
            pattern: /divide-(.+?)-(.+)/,
        },
        {
            pattern: /ring-(.+?)-(.+)/,
            variants: ["focus"]
        },
    ],
}
