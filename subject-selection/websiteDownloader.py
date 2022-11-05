import subprocess


def runcmd(cmd, verbose=False, *args, **kwargs):
    process = subprocess.Popen(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        shell=True
    )
    std_out, std_err = process.communicate()
    if verbose:
        print(std_out.strip(), std_err)
    pass


runcmd('wget --no-clobber --convert-links --recursive --level=1  --random-wait -p -E -e robots=off -U chrome www.science.org ')
