function test-draw-list ()
{
	 {
		  cat <<EOF
1
aaa
2
1
bbb
4
EOF
		  for idx in $(seq 1 5000) ; do
				cat <<EOF
2
1
EOF
		  done
		  cat <<EOF
0
EOF
	 } | ./debug-wv | grep DBG | cut -f 6 -d ' ' | sort | uniq -c
}

function test-update-weight ()
{
	 {
		  cat <<EOF
1
aaa
2
1
bbb
4
1
ccc
3
1
ddd
5
EOF
		  for idx2 in $(seq 1 20) ; do
				cat <<EOF
3
ccc
10
0.1
4
EOF
				done
		  for idx1 in $(seq 1 10) ; do
				for idx2 in $(seq 1 20) ; do
					 cat <<EOF
3
ccc
10
0.1
EOF
				done
				cat <<EOF
4
EOF
		  done
				cat <<EOF
0
EOF
	 } | ./debug-wv | grep DBG
}

test-update-weight