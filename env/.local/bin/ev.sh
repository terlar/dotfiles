#!/bin/bash
[ "${TRACE}" ] && set -x
set -eu
set -o pipefail

EVPATH="${EVPATH:-$HOME/.config/env}"

print_env() {
	local rel_varpath="${1}"
	local abs_varpath="${EVPATH}/${rel_varpath}"

	if [ -f "${abs_varpath}" ]
	then
		local varname="${rel_varpath##*/}"
		if [ -x "${abs_varpath}" ]
		then
			echo "${varname}"="$(${abs_varpath})"
		else
			echo "${varname}"="$(cat "${abs_varpath}")"
		fi
	elif [ -d "${abs_varpath}" ]
	then
		for i in "${abs_varpath}"/*
		do
			print_env "${i/${EVPATH}\//}"
		done
	else
		>&2 echo "ev: ${rel_varpath} is not a file or directory in EVPATH"
		return 1
	fi
}

main() {
	if [ ! -d "${EVPATH}" ]
	then
		>&2 echo "ev: EVPATH '${EVPATH}' is not a directory"
		return 1
	fi

	local target="${1?DIR or FILE not provided}"
	shift 1

	if [ $# -eq 0 ]
	then
		print_env "${target}"
	else
		# We want to split each env var	to prevent everything
		# being set into the first variable name.
		#
		# shellcheck disable=SC2046
		env $(print_env "${target}") "$@"
	fi
}

main "$@"
