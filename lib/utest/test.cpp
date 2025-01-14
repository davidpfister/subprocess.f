#include "pch.h"
#include "../src/subprocess.h"

TEST(utest, process_return_zero) {
	char* commandLine = "./process_return_zero";
	struct subprocess_s process;
	int ret = -1;
	subprocess_create(commandLine, 0, &process);

	ASSERT_EQ(1, process.alive);

	ASSERT_EQ(0, subprocess_join(&process, &ret));

	ASSERT_EQ(0, subprocess_destroy(&process));

	ASSERT_EQ(0, subprocess_destroy(&process));
}