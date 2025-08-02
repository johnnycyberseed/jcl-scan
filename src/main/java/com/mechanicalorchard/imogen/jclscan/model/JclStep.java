package com.mechanicalorchard.imogen.jclscan.model;

import lombok.Builder;
import lombok.Data;
import org.springframework.lang.Nullable;

@Data
@Builder
public class JclStep {
    private String name;
    @Nullable
    private Program pgm;
    @Nullable
    private JclProc proc;

    // TODO:
    // - Lint: ensure there's exactly one executable:
    //   - a first positional parameter,
    //   - a PROC, or
    //   - a PGM.

    // References:
    // [1] https://www.ibm.com/docs/en/zos-basic-skills?topic=do-jcl-exec-statements-positional-frequently-used-parameters
}