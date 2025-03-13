from bs4 import BeautifulSoup
from pathlib import Path
from dataclasses import dataclass
from warnings import warn
import re

@dataclass
class Test:
    number: int
    description: str
    result: str
    code: str
    should_error: bool = False

    def __str__(self):
        return f"""
<test>
<number>{self.number}</number>
<description>{self.description}</description>
<does-error>{'true' if self.should_error else 'false'}</does-error>
<result>{self.result}</result>
<code>
{self.code}
</code>
</test>
"""

def tests_html_str(title: str, tests: list[Test]):
    tests_str = ''
    for test in tests:
        tests_str += str(test)

    html_str = f"""<html>
<head>
<title>{title}</title>
</head>
<body>
{tests_str}
</body>
</html>"""

    return html_str


raw_html_dir = Path('raw_html')
raw_html_tests = list(raw_html_dir.iterdir())
raw_html_tests.sort()

normalized_html_dir = Path('html')

for raw_html_path in raw_html_tests:
    print(f'\n\n##### Processing {raw_html_path}... #####\n')
    with open(raw_html_path) as raw_html_file:
        soup = BeautifulSoup(raw_html_file, 'html.parser')
        with open(normalized_html_dir / raw_html_path.name, 'w') as normalized_html_file:
            title = soup.title.string
            print(f'Title: {title}')

            tests = []
            for p in soup.find_all('p'):
                if p.pre is None:
                    warn(f'Skipping paragraph: {p}')
                    continue  # This probably isn't a test

                meta: str = p.contents[0].string
                meta = meta.strip().replace('\n', ' ')
                number_matches = re.search('Test ([0-9]+)', meta)
                number = number_matches.group(1)

                should_error = 'error' in meta
                if should_error:
                    result = 'error'
                else:
                    return_matches = re.search('return (true|false|[-0-9]+)', meta)
                    result = return_matches.group(1)

                print(f'Meta: test {number} {"errors" if should_error else "returns"} "{result}" ({meta})')

                code = p.pre.string.strip()

                tests.append(Test(number, meta, result, code, should_error))

            normalized_html_file.write(tests_html_str(title, tests))
