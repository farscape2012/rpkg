#!/usr/bin/python

import networkx as nx
import json
import sys, getopt

def main(argv):
	in_file		= ""
	out_file	= ""
	source		= ""
	target		= ""

	if len(argv) !=8 and len(argv) != 1:
		print "argv was wrong."
		print argv
		print 'all_simple_paths.py -i <inputfile> -o <outputfile> -s <source> -t <target>'
		sys.exit(2)
	try:
		opts, args = getopt.getopt(argv, "h:i:o:s:t:",["ifile","ofile","source","target"])
	except getopt.GetoptError:
		print 'all_simple_paths.py -i <inputfile> -o <outputfile> -s <source> -t <target>'
		sys.exit(2)
	for opt, arg in opts:
		if opt == '-h':
			print 'all_simple_paths.py -i <inputfile> -o <outputfile> -s <source> -t <target>'
			sys.exit()
		elif opt in ('-i', "--ifile"):
			in_file = arg
		elif opt in ("-o", "--ofile"):
			out_file = arg
		elif opt in ("-s","--source"):
			source = arg
		elif opt in ("-t","--target"):
			target = arg
	G           = nx.read_graphml(path=in_file)
	name        = nx.get_node_attributes(G,'name')
	inv_name    = {v: k for k, v in name.items()}
	print "input\t:" + in_file
	print "output\t:" + out_file
	print "path\t:" + source + "-->" + target
	source      = inv_name[source]
	target      = inv_name[target]
	f           = open(out_file, 'w+a')
	for path in nx.all_simple_paths(G, source=source, target=target):
#		print path
		path=[name[x] for x in path]
		f.write(','.join(path))
		f.write('\n')
	f.close()

if __name__ == "__main__":
   main(sys.argv[1:])
