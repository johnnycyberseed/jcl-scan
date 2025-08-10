package com.mechanicalorchard.jclscan.model;

public sealed interface Program permits ProgramSummary, ProgramRef {
    String getName();

    enum Kind {
        ASSEMBLY("Assembler"),
        COBOL("COBOL"),
        EASYTRIEVE("Easytrieve");

        private final String label;

        Kind(String label) {
            this.label = label;
        }

        public String getLabel() {
            return label;
        }

        @Override
        public String toString() {
            return label;
        }
    }
}