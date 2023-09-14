#!/usr/bin/env python3
# coding=utf-8

import os, sys, os.path
import glob
import jinja2 as jj2
import jj2_context


if __name__ == '__main__':
    # assert len(sys.argv) > 1  # all arguments are target files

    lsp = '!#jj2' # os.environ['jj2_lsp']  # line statement 
    inDir = os.getenv('jj2_in',  sys.path[0])
    outDir = os.getenv("jj2_out", sys.path[0]+'/..')

    ctt = jj2_context.ctt

    files = glob.glob(inDir + "/*jj2")

    for infile in files:
        try:
            assert os.path.exists(infile), f'{infile} doesn''t exist'
            file_abspath = os.path.abspath(infile)

            inDir = os.path.dirname(file_abspath)

            fbname = os.path.basename(file_abspath)
            oName, ext = os.path.splitext(fbname)
            oPath = outDir + "/" + oName

            assert ext == '.jj2'

            content = open(infile).read()

            template = jj2.Template(content, line_statement_prefix=lsp)

            if ctt:
                content2 = template.render(ctt)
            else:
                content2 = template.render()
            # print(content2)
            with open(oPath, 'w') as f:
                f.write(content2)
        except:
            print(f'Error in {infile}')
            raise