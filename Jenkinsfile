pipeline {
    environment {
        CARGO_HOME = "${WORKSPACE}/.cargo"
    }

    agent {
        dockerfile {
            filename 'Jenkins.Dockerfile'
        }
    }
    stages {
        stage('Git') {
            steps {
                sh 'printenv'
                checkout scm
                sh 'git submodule update --init'
            }
        }
        stage('Build Release Binary') {
            steps {
	            cache(maxCacheSize: 10240, caches: [
	                [$class: 'ArbitraryFileCache', excludes: 'release/**', includes: '**', path: "${WORKSPACE}/mjrt-impl/target" ],
	                [$class: 'ArbitraryFileCache', excludes: 'release/build/libfirm-rs-bindings*', includes: '**', path: "${WORKSPACE}/target" ],
	                [$class: 'ArbitraryFileCache', excludes: '', includes: '**', path: "${WORKSPACE}/.cargo" ],
	            ]) {
                    // mjrt must always be rebuilt because it invokes `cargo build ../mjrt-impl`,
                    // and something about the rerun-if-changed mechanism is broken with cargo caching
                    sh 'cargo -vv clean --release -p mjrt'
					sh 'cargo build --release'
                    catchError {
                        sh 'cargo fmt --all -- --check'
                    }
                    catchError {
                        sh 'cargo clippy --release --all-targets --all-features -- -D warnings'
                    }
	            }
				fileExists 'target/release/compiler-cli'
            }
        }
		stage('Run normal tests') {
		    steps {
				catchError {
		    	    sh 'RUST_MIN_STACK=8388608 cargo test --release'
				}
		    }
		}
		stage('Run integration tests') {
		    steps {
				catchError {
		    	    sh 'COMPILER_BINARY="$(readlink -f ./target/release/compiler-cli)" cargo test --release --test integration'
				}
		    }
		}
		stage('Checkout mjtests') {
		    steps {
				sh 'rm -rf runmjtest'
				sh 'mkdir -p runmjtest'
				dir ('runmjtest') {
					sh 'git clone https://git.scc.kit.edu/IPDSnelting/mjtest.git'
					dir('mjtest') {
			    	    sh 'git submodule update --init --remote'
                    }
				}
            }
        }
        stage('mjtest lexer') {
            steps {
                dir ('runmjtest') {
                    catchError {
			            sh 'MJ_TIMEOUT=60 MJ_RUN=../target/release/compiler-cli python3 ./mjtest/mjt.py lexer'
                    }
                }
            }
        }
        stage('mjtest syntax') {
            steps {
                dir ('runmjtest') {
                    sh 'MJ_TIMEOUT=60 MJ_RUN=../target/release/compiler-cli python3 ./mjtest/mjt.py syntax'
                }
            }
        }
        stage('mjtest semantic') {
            steps {
                dir ('runmjtest') {
                    sh 'MJ_TIMEOUT=60 MJ_RUN=../target/release/compiler-cli python3 ./mjtest/mjt.py semantic'
                }
            }
        }
        stage('mjtest compile-firm') {
            steps {
                dir ('runmjtest') {
                    sh 'MJ_TIMEOUT=60 MJ_RUN=../target/release/compiler-cli python3 ./mjtest/mjt.py compile-firm'
                }
            }
        }
        // Enable when appropriate
        // stage('mjtest compile') {
        //     steps {
        //         dir ('runmjtest') {
        //             sh 'MJ_TIMEOUT=60 MJ_RUN=../target/release/compiler-cli python3 ./mjtest/mjt.py compile'
        //         }
        //     }
        // }
    }
}


